CLASS zcl_wasm_memory_copy DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
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

    DATA(lv_number) = io_memory->mi_stack->pop_i32( )->get_unsigned( ).
    DATA(lv_source) = io_memory->mi_stack->pop_i32( )->get_unsigned( ).
    DATA(lv_destination) = io_memory->mi_stack->pop_i32( )->get_unsigned( ).

    DATA(li_linear) = io_memory->mi_linear.
    IF lv_source + lv_number > li_linear->size_in_bytes( )
        OR lv_destination + lv_number > li_linear->size_in_bytes( ).
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_copy: out of bounds memory access'.
    ELSEIF lv_source < 0 OR lv_destination < 0 OR lv_number < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_copy: negative memory access'.
    ENDIF.

    IF lv_number = 0.
      RETURN.
    ENDIF.

    DATA(lv_bytes) = li_linear->get(
      iv_length = lv_number
      iv_offset = lv_source ).

    lv_bytes = zcl_wasm_binary_stream=>reverse_hex( lv_bytes ).

    li_linear->set(
      iv_offset = lv_destination
      iv_bytes  = lv_bytes ).

  ENDMETHOD.

ENDCLASS.
