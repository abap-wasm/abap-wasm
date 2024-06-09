CLASS zcl_wasm_global_set DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_globalidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_globalidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_global_set IMPLEMENTATION.

  METHOD constructor.
    mv_globalidx = iv_globalidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_global_set( io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-global-set-x

    io_memory->get_globals( )->set(
      iv_index = mv_globalidx
      ii_value = io_memory->mi_stack->pop( ) ).

  ENDMETHOD.

ENDCLASS.
