CLASS zcl_wasm_binary_stream DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_data TYPE xstring .
    METHODS get_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS get_data
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS peek
      IMPORTING
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS shift
      IMPORTING
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS shift_int
      RETURNING
        VALUE(rv_int) TYPE i .
    METHODS shift_u32
      RETURNING
        VALUE(rv_int) TYPE int8 .
    METHODS shift_f32
      RETURNING
        VALUE(rv_f) TYPE f .
    METHODS shift_f64
      RETURNING
        VALUE(rv_f) TYPE f .
    METHODS shift_i64
      RETURNING
        VALUE(rv_int) TYPE int8 .
    METHODS shift_utf8
      RETURNING
        VALUE(rv_name) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring .
ENDCLASS.



CLASS zcl_wasm_binary_stream IMPLEMENTATION.


  METHOD constructor.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD get_data.

    rv_data = mv_data.

  ENDMETHOD.


  METHOD get_length.

    rv_length = xstrlen( mv_data ).

  ENDMETHOD.


  METHOD peek.

    rv_data = mv_data(iv_length).

  ENDMETHOD.


  METHOD shift.

    rv_data = peek( iv_length ).

    mv_data = mv_data+iv_length.

  ENDMETHOD.

  METHOD shift_f64.

* https://en.wikipedia.org/wiki/Double-precision_floating-point_format

    DATA lv_hex TYPE x LENGTH 8.
    lv_hex = shift( 8 ).
    IF lv_hex = '0000000000000000'.
      RETURN.
    ENDIF.

    CASE lv_hex.
      WHEN '0000000000001040'.
        rv_f = 4.
      WHEN '0000000000000840'.
        rv_f = 3.
      WHEN '0000000000001C40'.
        rv_f = 7.
      WHEN '0000000000002440'.
        rv_f = 10.
      WHEN OTHERS.
        WRITE / lv_hex.
        ASSERT lv_hex = 'todo'.
    ENDCASE.

  ENDMETHOD.

  METHOD shift_f32.
* https://webassembly.github.io/spec/core/binary/values.html#binary-float
* Floating-point values are encoded directly by their IEEE 754

* https://en.wikipedia.org/wiki/Single-precision_floating-point_format
* https://cs.lmu.edu/~ray/demos/ieee754.html

    DATA lv_exponentx TYPE x LENGTH 1.
    DATA lv_exponent  TYPE i.
    DATA lv_fractionx TYPE x LENGTH 3.
    DATA lv_index     TYPE i.
    DATA lv_half      TYPE f VALUE 1.
    DATA lv_bit       TYPE c LENGTH 1.


    DATA lv_hex TYPE x LENGTH 4.
    lv_hex = shift( 4 ).
    IF lv_hex = '00000000'.
      RETURN.
    ENDIF.
    " WRITE: / 'input:', lv_hex.

    GET BIT 1 OF lv_hex INTO lv_bit.
    DATA(lv_sign) = lv_bit.

    DO 8 TIMES.
      lv_index = sy-index + 1.
      GET BIT lv_index OF lv_hex INTO lv_bit.
      lv_index = lv_index - 1.
      SET BIT lv_index OF lv_exponentx TO lv_bit.
    ENDDO.
    lv_exponent = lv_exponentx - 127.
    " WRITE: / 'exponent:', lv_exponent.

    DO 23 TIMES.
      lv_index = sy-index + 9.
      GET BIT lv_index OF lv_hex INTO lv_bit.
      lv_index = lv_index - 9 + 1.
      SET BIT lv_index OF lv_fractionx TO lv_bit.
    ENDDO.
* fix implicit 24th bit
    SET BIT 1 OF lv_fractionx TO 1.
    " WRITE: / 'fraction,hex:', lv_fractionx.

    DO 24 TIMES.
      GET BIT sy-index OF lv_fractionx INTO lv_bit.
      IF lv_bit = '1'.
        rv_f = rv_f + lv_half.
      ENDIF.
      lv_half = lv_half / 2.
    ENDDO.

    rv_f = rv_f * ( 2 ** lv_exponent ).

    " WRITE / rv_f.

  ENDMETHOD.

  METHOD shift_int.

* todo, this should be LEB128
* todo, deprecate this method, use the typed methods instead
* https://webassembly.github.io/spec/core/binary/values.html#binary-int

    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = shift( 1 ).
    rv_int = CONV i( lv_hex ).

  ENDMETHOD.

  METHOD shift_u32.

* https://webassembly.github.io/spec/core/binary/values.html#binary-int
* https://en.wikipedia.org/wiki/LEB128

    DATA lv_hex   TYPE x LENGTH 1.
    DATA lv_bit   TYPE c LENGTH 1.
    DATA lv_shift TYPE int8 VALUE 1.

    DO.
      lv_hex = shift( 1 ).

      GET BIT 1 OF lv_hex INTO lv_bit.
      SET BIT 1 OF lv_hex TO 0.

      rv_int = rv_int + CONV i( lv_hex ) * lv_shift.

      IF lv_bit = '0'.
        RETURN.
      ENDIF.

      lv_shift = lv_shift * 128.
    ENDDO.

  ENDMETHOD.

  METHOD shift_i64.

    DATA lv_hex   TYPE x LENGTH 1.
    DATA lv_bit   TYPE c LENGTH 1.
    DATA lv_shift TYPE i VALUE 1.

    DO.
      lv_hex = shift( 1 ).

      GET BIT 1 OF lv_hex INTO lv_bit.
      SET BIT 1 OF lv_hex TO 0.

      rv_int = rv_int + CONV i( lv_hex ) * lv_shift.

      IF lv_bit = '0'.
        GET BIT 2 OF lv_hex INTO lv_bit.
        IF lv_bit = '1'.
          rv_int = 0 - rv_int.
        ENDIF.
        RETURN.
      ENDIF.

      lv_shift = lv_shift * 128.
    ENDDO.

  ENDMETHOD.

  METHOD shift_utf8.

* https://webassembly.github.io/spec/core/binary/values.html#names

    DATA lo_conv TYPE REF TO object.

    DATA(lv_xstr) = shift( shift_int( ) ).

    TRY.
        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
          RECEIVING
            instance = lo_conv.

        CALL METHOD lo_conv->('IF_ABAP_CONV_IN~CONVERT')
          EXPORTING
            source = lv_xstr
          RECEIVING
            result = rv_name.
      CATCH cx_sy_dyn_call_illegal_class.
        DATA(lv_conv_in_class) = 'CL_ABAP_CONV_IN_CE'.
        CALL METHOD (lv_conv_in_class)=>create
          EXPORTING
            encoding = 'UTF-8'
          RECEIVING
            conv     = lo_conv.

        CALL METHOD lo_conv->('CONVERT')
          EXPORTING
            input = lv_xstr
          IMPORTING
            data  = rv_name.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
