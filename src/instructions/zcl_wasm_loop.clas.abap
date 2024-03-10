CLASS zcl_wasm_loop DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type TYPE xstring
        it_in         TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_block_type   TYPE xstring.
    DATA mt_instructions TYPE zif_wasm_instruction=>ty_list.
ENDCLASS.

CLASS zcl_wasm_loop IMPLEMENTATION.

  METHOD constructor.
    mv_block_type   = iv_block_type.
    mt_instructions = it_in.
  ENDMETHOD.

  METHOD parse.

    DATA(lv_block_type) = io_body->shift( 1 ).
    DATA(lo_parser) = NEW zcl_wasm_parser( ).

    zcl_wasm_instructions=>parse(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_instructions) ).

    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'loop: expected end opcode'.
    ENDIF.

    ri_instruction = NEW zcl_wasm_loop(
      iv_block_type = lv_block_type
      it_in         = lt_instructions ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* loops doesnt loop, but branches to the start instead of block branches which branches to the end

    DATA(lo_block) = NEW zcl_wasm_block_helper(
      iv_block_type = mv_block_type
      io_module     = io_module ).
    lo_block->start( io_memory ).

    DO.
      TRY.
          rv_control = io_module->execute_instructions( mt_instructions ).

          IF rv_control = zif_wasm_instruction=>c_control-return_.
            RETURN.
          ENDIF.
        CATCH zcx_wasm_branch INTO DATA(lx_branch).
          IF lx_branch->depth = 0.
            CONTINUE.
          ENDIF.
          RAISE EXCEPTION TYPE zcx_wasm_branch EXPORTING depth = lx_branch->depth - 1.
      ENDTRY.

      EXIT.
    ENDDO.

    lo_block->end( io_memory ).

  ENDMETHOD.

ENDCLASS.
