CLASS zcl_wasm_ref_null DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_ref_type TYPE xstring.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_ref_type TYPE xstring.
ENDCLASS.

CLASS zcl_wasm_ref_null IMPLEMENTATION.

  METHOD constructor.
    mv_ref_type = iv_ref_type.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_ref_null( io_body->shift( 1 ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-ref-mathsf-ref-null-t

    CASE mv_ref_type.
      WHEN zif_wasm_types=>c_reftype-funcref.
        io_memory->mi_stack->push( NEW zcl_wasm_funcref( -1 ) ).
      WHEN zif_wasm_types=>c_reftype-externref.
        io_memory->mi_stack->push( NEW zcl_wasm_externref( -1 ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_ref_null: Unknown ref type { mv_ref_type }|.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
