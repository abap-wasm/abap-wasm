CLASS zcl_wasm_table_fill DEFINITION PUBLIC.
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

CLASS zcl_wasm_table_fill IMPLEMENTATION.

  METHOD constructor.
    mv_tableidx = iv_tableidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_table_fill(
      iv_tableidx = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-table-mathsf-table-fill-x

    DATA(lv_n) = io_memory->mi_stack->pop_i32( )->mv_value.
    DATA(lv_val) = io_memory->mi_stack->pop( ).
    DATA(lv_i) = io_memory->mi_stack->pop_i32( )->mv_value.

    IF lv_i + lv_n > io_memory->table_size( CONV #( mv_tableidx ) ).
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_table_fill: out of bounds'.
    ENDIF.

    DO lv_n TIMES.
      io_memory->table_set(
        iv_tableidx = CONV #( mv_tableidx )
        iv_offset   = lv_i + sy-index - 1
        ii_value    = lv_val ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
