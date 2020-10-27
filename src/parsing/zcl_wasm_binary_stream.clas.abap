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
    METHODS peek
      IMPORTING
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS shift
      IMPORTING
        !iv_length TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring .
ENDCLASS.



CLASS ZCL_WASM_BINARY_STREAM IMPLEMENTATION.


  METHOD constructor.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD get_length.

    rv_length = xstrlen( mv_data ).

  ENDMETHOD.


  METHOD peek.

    rv_data = mv_data(iv_length).

  ENDMETHOD.


  METHOD shift.

    mv_data = mv_data+iv_length.

  ENDMETHOD.
ENDCLASS.
