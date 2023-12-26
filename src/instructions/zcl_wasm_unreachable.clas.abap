CLASS zcl_wasm_unreachable DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_unreachable IMPLEMENTATION.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'trap'.
  ENDMETHOD.

ENDCLASS.