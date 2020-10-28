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
    METHODS shift_utf8
      RETURNING
        VALUE(rv_name) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring .
ENDCLASS.



CLASS ZCL_WASM_BINARY_STREAM IMPLEMENTATION.


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
* https://webassembly.github.io/spec/core/binary/values.html#binary-int

    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = shift( 1 ).
    rv_int = CONV i( lv_hex ).

  ENDMETHOD.


  METHOD shift_utf8.

    DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.
    DATA(lv_length) = shift_int( ).
    DATA(lv_binary) = shift( lv_length ).

    lo_conv = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert(
      EXPORTING input = lv_binary
      IMPORTING data = rv_name ).

  ENDMETHOD.
ENDCLASS.
