CLASS zcl_wasm_table_set DEFINITION PUBLIC.
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

CLASS zcl_wasm_table_set IMPLEMENTATION.

  METHOD constructor.
    mv_tableidx = iv_tableidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_table_set(
      iv_tableidx = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-table-mathsf-table-set-x

    DATA(li_val) = io_memory->mi_stack->pop( ).
    IF li_val->get_type( ) <> zif_wasm_types=>c_reftype-funcref
        AND li_val->get_type( ) <> zif_wasm_types=>c_reftype-externref.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_table_set: expected reference type'.
    ENDIF.

    DATA(lv_i) = io_memory->mi_stack->pop_i32( )->mv_value.
    IF lv_i < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_table_set: negative index'.
    ENDIF.

    io_memory->table_set(
      iv_tableidx = CONV #( mv_tableidx )
      iv_offset   = lv_i
      ii_value    = li_val ).

  ENDMETHOD.

ENDCLASS.
