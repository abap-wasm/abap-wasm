CLASS zcl_wasm_memory_copy DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_memory_copy IMPLEMENTATION.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_memory_copy( ).
    ASSERT io_body->shift( 1 ) = '00'.
    ASSERT io_body->shift( 1 ) = '00'.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-copy

    DATA(lv_number) = io_memory->stack_pop_i32( )->get_signed( ).
    IF lv_number < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory_copy: out of bounds memory access' ).
    ENDIF.
    DATA(lv_source) = io_memory->stack_pop_i32( )->get_signed( ).
    IF lv_source < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory_copy: out of bounds memory access' ).
    ENDIF.
    DATA(lv_destination) = io_memory->stack_pop_i32( )->get_signed( ).
    IF lv_destination < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory_copy: out of bounds memory access' ).
    ENDIF.

    IF lv_source + lv_number > io_memory->get_linear( )->size_in_bytes( )
        OR lv_destination + lv_number > io_memory->get_linear( )->size_in_bytes( ).
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory_copy: out of bounds memory access' ).
    ENDIF.

* todo,
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_memory_copy' ).

  ENDMETHOD.

ENDCLASS.
