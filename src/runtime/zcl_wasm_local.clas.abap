CLASS zcl_wasm_local DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
        !iv_index  TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_LOCAL IMPLEMENTATION.


  METHOD get.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-local-get-x

    io_memory->stack_push( io_memory->local_get( iv_index ) ).

  ENDMETHOD.
ENDCLASS.
