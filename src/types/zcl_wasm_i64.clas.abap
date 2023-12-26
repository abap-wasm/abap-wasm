CLASS zcl_wasm_i64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_I64 IMPLEMENTATION.


  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_types=>c_value_type-i64.

  ENDMETHOD.
ENDCLASS.
