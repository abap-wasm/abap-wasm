CLASS zcl_wasm_ref_is_null DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_ref_is_null IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_ref_is_null( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-ref-mathsf-ref-is-null

    DATA lv_null TYPE abap_bool.

    DATA(li_value) = io_memory->mi_stack->pop( ).

    "##feature-start=debug
    IF li_value IS INITIAL.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_ref_is_null: initial value popped|.
    ENDIF.
    "##feature-end=debug

    CASE li_value->get_type( ).
      WHEN zif_wasm_types=>c_reftype-externref.
        lv_null = CAST zcl_wasm_externref( li_value )->is_null( ).
      WHEN zif_wasm_types=>c_reftype-funcref.
        lv_null = CAST zcl_wasm_funcref( li_value )->is_null( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_ref_is_null: Expected ref, got { li_value->get_type( ) }|.
    ENDCASE.

    IF lv_null = abap_true.
      io_memory->mi_stack->push( zcl_wasm_i32=>gc_one ).
    ELSE.
      io_memory->mi_stack->push( zcl_wasm_i32=>gc_zero ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
