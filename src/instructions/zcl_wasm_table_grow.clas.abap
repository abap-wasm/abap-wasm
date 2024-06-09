CLASS zcl_wasm_table_grow DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_tableidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
  PRIVATE SECTION.
    DATA mv_tableidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_table_grow IMPLEMENTATION.

  METHOD constructor.
    mv_tableidx = iv_tableidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_table_grow(
      iv_tableidx = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-table-mathsf-table-grow-x

    DATA(lv_sz) = io_memory->table_size( CONV #( mv_tableidx ) ).

    DATA(lv_n) = io_memory->mi_stack->pop_i32( )->mv_value.
    DATA(lv_val) = io_memory->mi_stack->pop( ).

    DATA(lv_max) = io_memory->table_get_max( CONV #( mv_tableidx ) ).
    IF lv_max > 0 AND lv_n + io_memory->table_size( CONV #( mv_tableidx ) ) > lv_max.
      io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( -1 ) ).
    ELSE.
      io_memory->table_grow(
        iv_tableidx = CONV #( mv_tableidx )
        iv_count    = lv_n
        ii_value    = lv_val ).
      io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_sz ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
