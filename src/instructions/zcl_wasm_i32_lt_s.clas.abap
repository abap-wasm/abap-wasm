CLASS zcl_wasm_i32_lt_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_lt_s IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>lt_s( io_memory ).
  ENDMETHOD.

ENDCLASS.