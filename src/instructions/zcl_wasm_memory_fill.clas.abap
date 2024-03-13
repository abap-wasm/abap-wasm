CLASS zcl_wasm_memory_fill DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
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
    DATA(li_linear) = io_memory->get_linear( ).
    DATA(lv_n) = io_memory->get_stack( )->pop_i32( )->get_signed( ).
    DATA(lo_val) = io_memory->get_stack( )->pop_i32( ).
    DATA(lo_d) = io_memory->get_stack( )->pop_i32( ).

    IF lv_n + lo_d->get_signed( ) > li_linear->size_in_bytes( ).
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'memory_fill: trap'.
    ELSEIF lv_n < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'memory_fill: trap'.
    ELSEIF lv_n = 0.
      RETURN.
    ENDIF.

    IF lv_n > 1000.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_fill, refactor to iteration instead of recursion'.
    ENDIF.

    lv_offset = lo_d->get_signed( ).
    lv_hex = lo_val->get_signed( ).

    DO lv_n TIMES.
      io_memory->get_linear( )->set(
        iv_offset = lv_offset
        iv_bytes  = lv_hex ).
      lv_offset = lv_offset + 1.
    ENDDO.

"     io_memory->get_stack( )->push( lo_d ).
"     io_memory->get_stack( )->push( lo_val ).

" * todo: refactor to iteration instead of recursion
"     NEW zcl_wasm_i32_store8(
"       iv_align  = 0
"       iv_offset = 0 )->zif_wasm_instruction~execute(
"       io_memory = io_memory
"       io_module = io_module ).

" * todo, Assert: due to the earlier check against the memory size,

"     io_memory->get_stack( )->push( zcl_wasm_i32=>from_signed( lo_d->get_signed( ) + 1 ) ).
"     io_memory->get_stack( )->push( lo_val ).
"     io_memory->get_stack( )->push( zcl_wasm_i32=>from_signed( lo_n->get_signed( ) - 1 ) ).

"     zif_wasm_instruction~execute(
"       io_memory = io_memory
"       io_module = io_module ).
  ENDMETHOD.

ENDCLASS.
