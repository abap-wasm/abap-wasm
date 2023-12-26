CLASS zcl_wasm_call DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_funcidx TYPE i.

  PRIVATE SECTION.
    DATA mv_funcidx TYPE i.
ENDCLASS.

CLASS zcl_wasm_call IMPLEMENTATION.

  METHOD constructor.
    mv_funcidx = iv_funcidx.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.