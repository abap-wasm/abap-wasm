CLASS zcl_wasm_local_get DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_index TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_index TYPE i.
ENDCLASS.



CLASS zcl_wasm_local_get IMPLEMENTATION.

  METHOD constructor.
    mv_index = iv_index.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-local-get-x

    io_memory->stack_push( io_memory->local_get( mv_index ) ).

  ENDMETHOD.
ENDCLASS.
