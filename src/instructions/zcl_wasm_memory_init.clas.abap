CLASS zcl_wasm_memory_init DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_dataidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING   zcx_wasm.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_dataidx TYPE int8.
ENDCLASS.



CLASS zcl_wasm_memory_init IMPLEMENTATION.


  METHOD constructor.
    mv_dataidx = iv_dataidx.
  ENDMETHOD.


  METHOD parse.
    ri_instruction = NEW zcl_wasm_memory_init( io_body->shift_u32( ) ).
    ASSERT io_body->shift( 1 ) = '00'.
  ENDMETHOD.


  METHOD zif_wasm_instruction~execute.
* copy passive data to memory

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-init-x

    DATA(li_stack) = io_memory->mi_stack.
    DATA(lv_length) = li_stack->pop_i32( )->get_unsigned( ).
    DATA(lv_offset) = li_stack->pop_i32( )->get_unsigned( ).
    DATA(lv_destination) = li_stack->pop_i32( )->get_unsigned( ).

    DATA(lv_bytes) = io_module->get_data_section( )->get_passive( mv_dataidx ).
    DATA(li_linear) = io_memory->mi_linear.

    IF lv_length + lv_offset > xstrlen( lv_bytes )
        OR lv_destination + lv_length > li_linear->size_in_bytes( ).
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_init: out of bounds memory access'.
    ENDIF.

    lv_bytes = lv_bytes+lv_offset(lv_length).

    li_linear->set(
      iv_offset = lv_destination
      iv_bytes  = lv_bytes ).

  ENDMETHOD.
ENDCLASS.
