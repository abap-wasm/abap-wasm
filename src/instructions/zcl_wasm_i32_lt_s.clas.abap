CLASS zcl_wasm_i32_lt_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_lt_s IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.