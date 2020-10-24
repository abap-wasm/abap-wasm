CLASS zcl_wasm_function DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_functions TYPE STANDARD TABLE OF REF TO zcl_wasm_function WITH DEFAULT KEY .

    METHODS get_export_name
      RETURNING
        VALUE(rv_export_name) TYPE string .
    METHODS constructor
      IMPORTING
        !it_instructions TYPE zcl_wasm_instructions=>ty_instructions
        !iv_export_name  TYPE string .
    METHODS get_instructions
      RETURNING
        VALUE(rt_instructions) TYPE zcl_wasm_instructions=>ty_instructions .
  PROTECTED SECTION.

    DATA mt_instructions TYPE zcl_wasm_instructions=>ty_instructions .
    DATA mv_export_name TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_FUNCTION IMPLEMENTATION.


  METHOD constructor.
    mt_instructions = it_instructions.
    mv_export_name = iv_export_name.
  ENDMETHOD.


  METHOD get_export_name.
    rv_export_name = mv_export_name.
  ENDMETHOD.


  METHOD get_instructions.
    rt_instructions = mt_instructions.
  ENDMETHOD.
ENDCLASS.
