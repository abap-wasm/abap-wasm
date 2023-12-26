CLASS zcl_wasm_local_get DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_localidx TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_localidx TYPE i.
ENDCLASS.



CLASS zcl_wasm_local_get IMPLEMENTATION.

  METHOD constructor.
    mv_localidx = iv_localidx.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-local-get-x

    io_memory->stack_push( io_memory->local_get( mv_localidx ) ).

  ENDMETHOD.
ENDCLASS.
