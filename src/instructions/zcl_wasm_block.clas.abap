CLASS zcl_wasm_block DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type   TYPE xstring
        it_instructions TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_block_type   TYPE xstring.
    DATA mt_instructions TYPE zif_wasm_instruction=>ty_list.

ENDCLASS.

CLASS zcl_wasm_block IMPLEMENTATION.

  METHOD constructor.
* https://webassembly.github.io/spec/core/valid/types.html#block-types
    mv_block_type   = iv_block_type.
    mt_instructions = it_instructions.
  ENDMETHOD.

  METHOD parse.

* Block types are encoded in special compressed form, by either the byte
* indicating the empty type, as a single value type, or as a type index
* encoded as a positive signed integer.
    DATA(lv_block_type) = io_body->shift( 1 ).

    zcl_wasm_instructions=>parse(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_instructions) ).

    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |block, expected end|.
    ENDIF.

    ri_instruction = NEW zcl_wasm_block(
      iv_block_type   = lv_block_type
      it_instructions = lt_instructions ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-block-xref-syntax-instructions-syntax-blocktype-mathit-blocktype-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-control-mathsf-end

* todo: block type?
    " WRITE / mv_block_type.
    CASE mv_block_type.
      WHEN zcl_wasm_types=>c_empty_block_type.
        " todo
      WHEN zcl_wasm_types=>c_value_type-i32.
        " i32
      WHEN zcl_wasm_types=>c_value_type-i64.
        " i64
      WHEN zcl_wasm_types=>c_value_type-f32.
        " f32
      WHEN zcl_wasm_types=>c_value_type-f64.
        " f64
      WHEN zcl_wasm_types=>c_vector_type.
        " todo
      WHEN zcl_wasm_types=>c_reftype-funcref.
        " todo
      WHEN zcl_wasm_types=>c_reftype-externref.
        " todo
      WHEN OTHERS.
        " todo
    ENDCASE.

    TRY.
        rv_control = NEW zcl_wasm_vm(
          io_memory = io_memory
          io_module = io_module )->execute( mt_instructions ).
      CATCH zcx_wasm_branch INTO DATA(lx_branch).
        IF lx_branch->depth > 0.
          RAISE EXCEPTION TYPE zcx_wasm_branch EXPORTING depth = lx_branch->depth - 1.
        ENDIF.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
