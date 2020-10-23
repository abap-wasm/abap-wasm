CLASS zcl_wasm_function DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING it_instructions TYPE zcl_wasm_instructions=>ty_instructions.
    METHODS get_instructions
      RETURNING VALUE(rt_instructions) TYPE zcl_wasm_instructions=>ty_instructions.
  PROTECTED SECTION.
    DATA mt_instructions TYPE zcl_wasm_instructions=>ty_instructions.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_FUNCTION IMPLEMENTATION.


  METHOD constructor.
    mt_instructions = it_instructions.
  ENDMETHOD.


  METHOD get_instructions.
    rt_instructions = mt_instructions.
  ENDMETHOD.
ENDCLASS.
