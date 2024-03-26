CLASS zcl_wasm_local_get DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_localidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING
        !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_localidx TYPE int8.
ENDCLASS.



CLASS zcl_wasm_local_get IMPLEMENTATION.

  METHOD constructor.
    mv_localidx = iv_localidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_local_get( io_body->shift_u32( ) + 1 ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-local-get-x

    io_memory->mi_stack->push( io_memory->mi_frame->local_get( mv_localidx ) ).

  ENDMETHOD.
ENDCLASS.
