CLASS zcl_wasm_memory_init DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_dataidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
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

    DATA(lv_number) = io_memory->stack_pop_i32( )->get_unsigned( ).
    DATA(lv_source) = io_memory->stack_pop_i32( )->get_unsigned( ).
    DATA(lv_destination) = io_memory->stack_pop_i32( )->get_unsigned( ).

    DATA(li_linear) = io_memory->get_linear( ).
    IF lv_source + lv_number > li_linear->size_in_bytes( )
        OR lv_destination + lv_number > li_linear->size_in_bytes( ).
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory_copy: out of bounds memory access' ).
    ENDIF.

    DATA(lv_bytes) = io_module->get_data_section( )->get_passive( mv_dataidx ).

    li_linear->set(
      iv_offset = lv_destination
      iv_bytes  = lv_bytes(lv_number) ).

  ENDMETHOD.

ENDCLASS.
