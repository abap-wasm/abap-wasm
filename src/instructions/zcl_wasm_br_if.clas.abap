CLASS zcl_wasm_br_if DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_labelidx TYPE int8.

    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_labelidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_br_if IMPLEMENTATION.

  METHOD constructor.
    mv_labelidx = iv_labelidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_br_if( io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* Unlike with other index spaces, indexing of labels is relative by nesting depth, that is, label
* refers to the innermost structured control instruction enclosing the referring branch instruction,
* while increasing indices refer to those farther out.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-if-l

    DATA(li_value) = io_memory->mi_stack->pop( ).
    "##feature-start=debug
    IF li_value->get_type( ) <> zif_wasm_types=>c_value_type-i32.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_br_if: expected i32, got { li_value->get_type( ) }|.
    ENDIF.
    "##feature-end=debug

    IF CAST zcl_wasm_i32( li_value )->mv_value = 0.
      RETURN.
    ENDIF.

    cs_control-control = zif_wasm_instruction=>c_control-branch.
    cs_control-depth = mv_labelidx.
  ENDMETHOD.

ENDCLASS.
