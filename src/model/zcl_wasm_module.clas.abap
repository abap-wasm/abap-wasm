CLASS zcl_wasm_module DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_functions TYPE zcl_wasm_function=>ty_functions.
    METHODS get_functions
      RETURNING VALUE(rt_functions) TYPE zcl_wasm_function=>ty_functions.
  PROTECTED SECTION.
    DATA mt_functions TYPE zcl_wasm_function=>ty_functions.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_MODULE IMPLEMENTATION.


  METHOD constructor.
    mt_functions = it_functions.
  ENDMETHOD.


  METHOD get_functions.
    rt_functions = mt_functions.
  ENDMETHOD.
ENDCLASS.
