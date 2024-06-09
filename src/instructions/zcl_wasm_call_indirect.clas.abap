CLASS zcl_wasm_call_indirect DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_typeidx  TYPE int8
        iv_tableidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
  PRIVATE SECTION.
    DATA mv_typeidx  TYPE int8.
    DATA mv_tableidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_call_indirect IMPLEMENTATION.

  METHOD constructor.
    mv_typeidx  = iv_typeidx.
    mv_tableidx = iv_tableidx.
  ENDMETHOD.

  METHOD parse.

    DATA(lv_typeidx) = io_body->shift_u32( ).
    DATA(lv_tableidx) = io_body->shift_u32( ).

    ri_instruction = NEW zcl_wasm_call_indirect(
      iv_typeidx  = lv_typeidx
      iv_tableidx = lv_tableidx ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-call-indirect-x-y
* https://coinexsmartchain.medium.com/wasm-introduction-part-6-table-indirect-call-65ad0404b003

    DATA(lv_i) = io_memory->mi_stack->pop_i32( )->mv_value.

    DATA(li_value) = io_memory->table_get(
      iv_tableidx = CONV #( mv_tableidx )
      iv_offset   = lv_i ).
    IF li_value->get_type( ) <> zif_wasm_types=>c_reftype-funcref.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_call_indirect: not a funcref { li_value->get_type( ) }|.
    ENDIF.
    DATA(lo_ref) = CAST zcl_wasm_funcref( li_value ).

    IF lo_ref->is_null( ).
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_call_indirect: null reference, { mv_tableidx }, { lv_i }|.
    ENDIF.

    DATA(ls_type) = io_module->get_type_by_index( mv_typeidx ).
    DATA(ls_function) = io_module->get_function_by_index( lo_ref->get_address( ) ).
    DATA(ls_ftype) = io_module->get_type_by_index( CONV #( ls_function-typeidx ) ).

    IF ls_type <> ls_ftype.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_call_indirect type mismatch|.
    ENDIF.

    zcl_wasm_call=>invoke(
      iv_funcidx = lo_ref->get_address( )
      io_memory  = io_memory
      io_module  = io_module ).
  ENDMETHOD.

ENDCLASS.
