CLASS zcl_wasm_block DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type   TYPE xstring
        it_instructions TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING
        io_body               TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.

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

    "##feature-start=debug
    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |block, expected end|.
    ENDIF.
    "##feature-end=debug

    ri_instruction = NEW zcl_wasm_block(
      iv_block_type   = lv_block_type
      it_instructions = lt_instructions ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-block-xref-syntax-instructions-syntax-blocktype-mathit-blocktype-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instr

    DATA(lo_block) = NEW zcl_wasm_block_helper(
      iv_block_type = mv_block_type
      io_module     = io_module ).
    lo_block->start( io_memory ).

    io_module->execute_instructions(
          EXPORTING
            it_instructions = mt_instructions
          CHANGING
            cs_control      = cs_control ).

    IF cs_control-control = zif_wasm_instruction=>c_control-return_.
      RETURN.
    ELSEIF cs_control-control = zif_wasm_instruction=>c_control-branch.
      IF cs_control-depth > 0.
        cs_control-depth = cs_control-depth - 1.
        RETURN.
      ELSE.
        CLEAR cs_control-control.
      ENDIF.
    ENDIF.

    lo_block->end( io_memory ).

  ENDMETHOD.

ENDCLASS.
