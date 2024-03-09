CLASS zcl_wasm_local_tee DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_localidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_localidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_local_tee IMPLEMENTATION.

  METHOD constructor.
    mv_localidx = iv_localidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_local_tee( io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-local-tee-x

    DATA(lo_val) = io_memory->get_stack( )->stack_pop( ).

    io_memory->get_stack( )->stack_push( lo_val ).
    io_memory->get_stack( )->stack_push( lo_val ).

    NEW zcl_wasm_local_set( mv_localidx )->zif_wasm_instruction~execute(
      io_memory = io_memory
      io_module = io_module ).

  ENDMETHOD.

ENDCLASS.
