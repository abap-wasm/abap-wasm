CLASS zcl_wasm_binary_stream DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_data TYPE xsequence .

    METHODS get_length
      RETURNING
        VALUE(rv_remaining) TYPE i .
    METHODS peek
      IMPORTING
        !iv_length     TYPE numeric
      RETURNING
        VALUE(rv_data) TYPE xstring .

    METHODS shift
      IMPORTING
        !iv_length     TYPE numeric
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS shift_u32
      RETURNING
        VALUE(rv_int) TYPE int8
      RAISING
        zcx_wasm.
    METHODS shift_f32
      RETURNING
        VALUE(rv_f) TYPE f .
    METHODS shift_f64
      RETURNING
        VALUE(ro_float) TYPE REF TO zcl_wasm_f64 .
    METHODS shift_i64
      RETURNING
        VALUE(rv_int) TYPE int8
      RAISING
        zcx_wasm.
    METHODS shift_i32
      RETURNING
        VALUE(rv_int) TYPE i
      RAISING
        zcx_wasm.
    METHODS shift_utf8
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        zcx_wasm.

    CLASS-METHODS reverse_hex
      IMPORTING
        iv_hex        TYPE xsequence
      RETURNING
        VALUE(rv_hex) TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_data TYPE xstring .
    DATA mv_xstrlen TYPE i.
    DATA mv_pointer TYPE i.
ENDCLASS.



CLASS ZCL_WASM_BINARY_STREAM IMPLEMENTATION.


  METHOD constructor.
    mv_data = iv_data.
    mv_pointer = 0.
    mv_xstrlen = xstrlen( mv_data ).
  ENDMETHOD.


  METHOD get_length.

    rv_remaining = mv_xstrlen - mv_pointer.

  ENDMETHOD.


  METHOD peek.

    rv_data = mv_data+mv_pointer(iv_length).

  ENDMETHOD.


  METHOD reverse_hex.
* todo, will CHANGING make this method faster?
    DATA lv_hex    TYPE x LENGTH 1.
    DATA lv_char   TYPE c LENGTH 1.
    DATA lv_len    TYPE i.
    DATA lv_offset TYPE i.

    lv_len = xstrlen( iv_hex ).
    DO lv_len TIMES.
      lv_offset = lv_len - sy-index.
      lv_hex = iv_hex+lv_offset(1).
      CONCATENATE rv_hex lv_hex INTO rv_hex IN BYTE MODE.
    ENDDO.

  ENDMETHOD.


  METHOD shift.

    rv_data = mv_data+mv_pointer(iv_length).
    mv_pointer = mv_pointer + iv_length.

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

    lv_hex = reverse_hex( lv_hex ).

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

    IF lv_sign > 0.
      rv_f = 0 - rv_f.
    ENDIF.

  ENDMETHOD.


  METHOD shift_f64.
* in little endian order
* https://en.wikipedia.org/wiki/Double-precision_floating-point_format
* https://people.astro.umass.edu/~weinberg/a732/notes07_01.pdf

    DATA lv_exponentx TYPE x LENGTH 2. " 11 bits
    DATA lv_exponent  TYPE i.
    DATA lv_fractionx TYPE x LENGTH 7. " 52 bits
    DATA lv_index     TYPE i.
    DATA lv_half      TYPE f VALUE 1.
    DATA lv_bit       TYPE c LENGTH 1.
    DATA lv_f         TYPE f.

    DATA lv_hex TYPE x LENGTH 8.
    lv_hex = shift( 8 ).
    IF lv_hex = '0000000000000000'.
      ro_float = zcl_wasm_f64=>from_float( 0 ).
      RETURN.
    ENDIF.

    lv_hex = reverse_hex( lv_hex ).
    " WRITE: / 'reversed:', lv_hex.

    IF lv_hex = zcl_wasm_f64=>gc_special_hex-positive_infinity.
* positive infinity
      ro_float = zcl_wasm_f64=>get_positive_infinity( ).
      RETURN.
    ELSEIF lv_hex = zcl_wasm_f64=>gc_special_hex-negative_infinity.
* negative infinity
      ro_float = zcl_wasm_f64=>get_negative_infinity( ).
      RETURN.
    ELSEIF lv_hex = zcl_wasm_f64=>gc_special_hex-nan.
* NaN
      ro_float = zcl_wasm_f64=>get_nan( ).
      RETURN.
    ENDIF.

    GET BIT 1 OF lv_hex INTO lv_bit.
    DATA(lv_sign) = lv_bit.

    DO 11 TIMES.
      lv_index = sy-index + 1.
      GET BIT lv_index OF lv_hex INTO lv_bit.
      lv_index = lv_index - 1 + 5.
      SET BIT lv_index OF lv_exponentx TO lv_bit.
    ENDDO.
    " WRITE: / 'exponentx:', lv_exponentx.
    lv_exponent = lv_exponentx - 1023.
    " WRITE: / 'exponent:', lv_exponent.

    DO 52 TIMES.
      lv_index = sy-index + 12.
      GET BIT lv_index OF lv_hex INTO lv_bit.
      lv_index = lv_index - 12 + 1.
      SET BIT lv_index OF lv_fractionx TO lv_bit.
    ENDDO.
* fix implicit 24th bit
    SET BIT 1 OF lv_fractionx TO 1.
    " WRITE: / 'fraction,hex:', lv_fractionx.

    DO 24 TIMES.
      GET BIT sy-index OF lv_fractionx INTO lv_bit.
      IF lv_bit = '1'.
        lv_f = lv_f + lv_half.
      ENDIF.
      lv_half = lv_half / 2.
    ENDDO.

    lv_f = lv_f * ( 2 ** lv_exponent ).

    IF lv_sign > 0.
      lv_f = 0 - lv_f.
    ENDIF.

    ro_float = zcl_wasm_f64=>from_float( lv_f ).

  ENDMETHOD.


  METHOD shift_i32.

* https://webassembly.github.io/spec/core/binary/values.html#binary-int

* https://en.wikipedia.org/wiki/LEB128

    DATA lv_hex   TYPE x LENGTH 1.
    DATA lv_bit   TYPE c LENGTH 1.
    DATA lv_shift TYPE int8 VALUE 1.
    DATA lv_int   TYPE int8.

    DO.
      "##feature-start=debug
      IF sy-index > 5.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'integer representation too long'.
      ENDIF.
      "##feature-end=debug

      lv_hex = shift( 1 ).

      GET BIT 1 OF lv_hex INTO lv_bit.
      SET BIT 1 OF lv_hex TO 0.

      lv_int = lv_int + CONV i( lv_hex ) * lv_shift.

      IF lv_bit = '0'.
        GET BIT 2 OF lv_hex INTO lv_bit.
        IF lv_bit = '1'.
          lv_int = lv_int - lv_shift * 128.
        ENDIF.
        rv_int = lv_int.
        RETURN.
      ENDIF.

      lv_shift = lv_shift * 128.
    ENDDO.

    rv_int = lv_int.

  ENDMETHOD.


  METHOD shift_i64.

* https://en.wikipedia.org/wiki/LEB128

    DATA lv_hex   TYPE x LENGTH 1.
    DATA lv_hex8  TYPE x LENGTH 8.
    DATA lv_bit   TYPE c LENGTH 1.
    DATA lv_target TYPE i VALUE 64.
    DATA lv_read TYPE i VALUE 7.

    DO.
      " leb128 can be at most 10 bytes for 64 bits
      IF sy-index > 10.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'integer representation too long'.
      ENDIF.

      lv_hex = shift( 1 ).

      GET BIT 1 OF lv_hex INTO DATA(lv_continuation).
      IF lv_continuation = 0.
        lv_read = 6.
      ENDIF.

      DO lv_read TIMES.
        DATA(lv_source) = 9 - sy-index.
        GET BIT lv_source OF lv_hex INTO lv_bit.

        SET BIT lv_target OF lv_hex8 TO lv_bit.
        lv_target = lv_target - 1.
        IF lv_target < 1.
          EXIT.
        ENDIF.
      ENDDO.

      IF lv_continuation = 0.
        GET BIT 2 OF lv_hex INTO DATA(lv_sign).
        IF lv_sign = 1.
* perhaps doing a single BIT-XOR runs faster?
          WHILE lv_target >= 1.
            SET BIT lv_target OF lv_hex8 TO 1.
            lv_target = lv_target - 1.
          ENDWHILE.
        ENDIF.

        rv_int = lv_hex8.
        RETURN.
      ENDIF.
    ENDDO.

    ASSERT 1 = 'not possible'.

  ENDMETHOD.


  METHOD shift_u32.

* https://webassembly.github.io/spec/core/binary/values.html#binary-int
* https://en.wikipedia.org/wiki/LEB128

    CONSTANTS lc_128 TYPE i VALUE 128.

    DATA lv_val   TYPE i.
    DATA lv_shift TYPE int8 VALUE 1.

    DO.
      "##feature-start=debug
      IF sy-index > 5.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'integer representation too long'.
      ENDIF.
      "##feature-end=debug

      lv_val = shift( 1 ).

      IF lv_val >= lc_128.
* continuation bit is set
        lv_val = lv_val - lc_128.
        rv_int = rv_int + lv_val * lv_shift.
      ELSE.
        rv_int = rv_int + lv_val * lv_shift.
        RETURN.
      ENDIF.

      lv_shift = lv_shift * lc_128.
    ENDDO.

  ENDMETHOD.


  METHOD shift_utf8.

* https://webassembly.github.io/spec/core/binary/values.html#names

    DATA lo_conv TYPE REF TO object.

    DATA(lv_xstr) = shift( shift_u32( ) ).

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
