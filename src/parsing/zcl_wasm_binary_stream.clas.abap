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
        VALUE(rv_int) TYPE i .
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
    DATA lv_shift TYPE i VALUE 1.

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
    ASSERT 1 = 'todo'.
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
