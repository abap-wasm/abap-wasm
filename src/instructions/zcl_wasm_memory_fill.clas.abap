CLASS zcl_wasm_memory_fill DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_memory_fill IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_memory_fill( ).
    ASSERT io_body->shift( 1 ) = '00'.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-fill

    DATA(li_linear) = io_memory->get_linear( ).
    DATA(lv_n) = io_memory->stack_pop_i32( ).
    DATA(lv_val) = io_memory->stack_pop_i32( ).
    DATA(lv_d) = io_memory->stack_pop_i32( ).

    IF lv_n->get_signed( ) + lv_d->get_signed( ) > li_linear->size_in_bytes( ).
      RAISE EXCEPTION NEW zcx_wasm( text = 'memory_fill: trap' ).
    ELSEIF lv_n->get_signed( ) = 0.
      RETURN.
    ENDIF.

    IF lv_n->get_signed( ) > 1000.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory_fill, refactor to iteration instead of recursion' ).
    ENDIF.

    io_memory->stack_push( lv_d ).
    io_memory->stack_push( lv_val ).

* todo, call i32.store8
* todo

    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_memory_fill' ).
  ENDMETHOD.

ENDCLASS.
