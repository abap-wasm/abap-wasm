CLASS zcl_wasm_i32_add DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_add IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.