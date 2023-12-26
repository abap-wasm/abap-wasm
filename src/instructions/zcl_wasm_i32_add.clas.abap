CLASS zcl_wasm_i32_add DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_add IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>add( io_memory ).
  ENDMETHOD.

ENDCLASS.