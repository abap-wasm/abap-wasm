CLASS zcl_wasm_end DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_end IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.