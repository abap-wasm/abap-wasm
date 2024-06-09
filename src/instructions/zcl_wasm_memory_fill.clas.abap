CLASS zcl_wasm_memory_fill DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_memory_fill IMPLEMENTATION.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_memory_fill( ).
    ASSERT io_body->shift( 1 ) = '00'.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-fill

    DATA lv_offset TYPE int8.
    DATA lv_hex TYPE x LENGTH 1.
    DATA(li_linear) = io_memory->mi_linear.
    DATA(lv_n) = io_memory->mi_stack->pop_i32( )->mv_value.
    DATA(lo_val) = io_memory->mi_stack->pop_i32( ).
    DATA(lv_d) = io_memory->mi_stack->pop_i32( )->mv_value.

    IF lv_n + lv_d > li_linear->size_in_bytes( ).
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'memory_fill: trap, larger than linear'.
    ELSEIF lv_n < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'memory_fill: trap, negative'.
    ELSEIF lv_n = 0.
      RETURN.
    ENDIF.

    IF lv_n > 70000.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_fill, too many bytes'.
    ENDIF.

    lv_offset = lv_d.
    lv_hex = lo_val->mv_value.

    DO lv_n TIMES.
      li_linear->set(
        iv_offset = lv_offset
        iv_bytes  = lv_hex ).
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
