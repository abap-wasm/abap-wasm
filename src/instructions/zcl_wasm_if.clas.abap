CLASS zcl_wasm_if DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type TYPE xstring
        it_in1        TYPE zif_wasm_instruction=>ty_list
        it_in2        TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING
        !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.
  PRIVATE SECTION.
    DATA mv_block_type TYPE xstring.
    DATA mt_in1        TYPE zif_wasm_instruction=>ty_list.
    DATA mt_in2        TYPE zif_wasm_instruction=>ty_list.
ENDCLASS.

CLASS zcl_wasm_if IMPLEMENTATION.

  METHOD constructor.
    mv_block_type = iv_block_type.
    mt_in1        = it_in1.
    mt_in2        = it_in2.
  ENDMETHOD.

  METHOD parse.

    DATA(lv_block_type) = io_body->shift( 1 ).

    zcl_wasm_instructions=>parse(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_in1) ).

    IF lv_last_opcode = zif_wasm_opcodes=>c_opcodes-else_.
      zcl_wasm_instructions=>parse(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = lv_last_opcode
          et_instructions = DATA(lt_in2) ).
    ENDIF.

    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |if parse: expected end|.
    ENDIF.

    ri_instruction = NEW zcl_wasm_if(
      iv_block_type = lv_block_type
      it_in1        = lt_in1
      it_in2        = lt_in2 ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#control-instructions
* https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
* https://webassembly.github.io/spec/core/binary/instructions.html#binary-blocktype

    DATA(lv_value) = io_memory->mi_stack->pop_i32( )->get_signed( ).

    DATA(lo_block) = NEW zcl_wasm_block_helper(
      iv_block_type = mv_block_type
      io_module     = io_module ).
    lo_block->start( io_memory ).

    TRY.
* If c is non-zero, then enter
        IF lv_value <> 0.
          rv_control = io_module->execute_instructions( mt_in1 ).
        ELSE.
          rv_control = io_module->execute_instructions( mt_in2 ).
        ENDIF.
        IF rv_control = zif_wasm_instruction=>c_control-return_.
          RETURN.
        ENDIF.
      CATCH zcx_wasm_branch INTO DATA(lx_branch).
        IF lx_branch->depth > 0.
          RAISE EXCEPTION TYPE zcx_wasm_branch EXPORTING depth = lx_branch->depth - 1.
        ENDIF.
    ENDTRY.

    lo_block->end( io_memory ).

  ENDMETHOD.

ENDCLASS.
