CLASS zcl_wasm_br_table DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    TYPES ty_branches TYPE STANDARD TABLE OF int8 WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        it_branches TYPE ty_branches
        iv_default  TYPE int8.

    CLASS-METHODS parse
      IMPORTING
        !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
        RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mt_branches TYPE ty_branches.
    DATA mv_default  TYPE int8.
ENDCLASS.

CLASS zcl_wasm_br_table IMPLEMENTATION.

  METHOD constructor.
    mt_branches = it_branches.
    mv_default  = iv_default.
  ENDMETHOD.

  METHOD parse.

    DATA lt_branches TYPE ty_branches.

    DATA(lv_length) = io_body->shift_u32( ).
    DO lv_length TIMES.
      APPEND io_body->shift_u32( ) TO lt_branches.
    ENDDO.

    DATA(lv_default) = io_body->shift_u32( ).

    ri_instruction = NEW zcl_wasm_br_table(
      it_branches = lt_branches
      iv_default  = lv_default ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-table-l-ast-l-n

    DATA(li_value) = io_memory->mi_stack->pop( ).
    "##feature-start=debug
    IF li_value->get_type( ) <> zif_wasm_types=>c_value_type-i32.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_br_table: expected i32, got { li_value->get_type( ) }|.
    ENDIF.
    "##feature-end=debug

* todo, this has to be get_unsigned() ?
    DATA(lv_i) = CAST zcl_wasm_i32( li_value )->mv_value.
    lv_i = lv_i + 1.

    READ TABLE mt_branches INDEX lv_i INTO DATA(lv_branch).
    IF sy-subrc <> 0.
      lv_branch = mv_default.
    ENDIF.

    cs_control-control = zif_wasm_instruction=>c_control-branch.
    cs_control-depth = lv_branch.
  ENDMETHOD.

ENDCLASS.
