CLASS zcl_wasm_i32_sub DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_sub IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>sub( io_memory ).
  ENDMETHOD.

ENDCLASS.