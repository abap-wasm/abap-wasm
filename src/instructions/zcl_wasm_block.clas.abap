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

    METHODS fix_stack
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory
        iv_length TYPE i
      RAISING
        zcx_wasm.

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

  METHOD fix_stack.
    IF mv_block_type <> zcl_wasm_types=>c_empty_block_type.
* runtime validations?
      DATA(lo_val) = io_memory->stack_pop( ).
    ENDIF.
    WHILE io_memory->stack_length( ) > iv_length.
      io_memory->stack_pop( ).
    ENDWHILE.
    IF lo_val IS NOT INITIAL.
      io_memory->stack_push( lo_val ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-block-xref-syntax-instructions-syntax-blocktype-mathit-blocktype-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-control-mathsf-end

    DATA(lv_length) = io_memory->stack_length( ).

    TRY.
        rv_control = NEW zcl_wasm_vm(
          io_memory = io_memory
          io_module = io_module )->execute( mt_instructions ).
        " fix_stack( io_memory = io_memory
        "            iv_length = lv_length ).
      CATCH zcx_wasm_branch INTO DATA(lx_branch).
        fix_stack( io_memory = io_memory
                   iv_length = lv_length ).
        IF lx_branch->depth > 0.
          RAISE EXCEPTION TYPE zcx_wasm_branch EXPORTING depth = lx_branch->depth - 1.
        ENDIF.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
