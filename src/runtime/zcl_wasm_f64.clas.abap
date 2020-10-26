CLASS zcl_wasm_f64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_F64 IMPLEMENTATION.


  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_value_types=>c_type-f64.

  ENDMETHOD.
ENDCLASS.
