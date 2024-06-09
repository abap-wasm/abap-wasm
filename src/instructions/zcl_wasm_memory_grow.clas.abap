CLASS zcl_wasm_memory_grow DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_memory_grow IMPLEMENTATION.

  METHOD parse.
    IF io_body->shift( 1 ) <> '00'.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_memory_grow->parse()|.
    ENDIF.
    ri_instruction = NEW zcl_wasm_memory_grow( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-grow

    DATA(lv_sz) = io_memory->mi_linear->size_in_pages( ).

    DATA(lv_pages) = io_memory->mi_stack->pop_i32( )->get_unsigned( ).
    " WRITE / |memory.grow { lv_pages } pages|.

    io_memory->mi_linear->grow( lv_pages ).

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_sz ) ).

  ENDMETHOD.

ENDCLASS.
