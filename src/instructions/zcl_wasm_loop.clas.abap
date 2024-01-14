CLASS zcl_wasm_loop DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type TYPE xstring
        it_in         TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_block_type TYPE xstring.
    DATA mt_in         TYPE zif_wasm_instruction=>ty_list.
ENDCLASS.

CLASS zcl_wasm_loop IMPLEMENTATION.

  METHOD constructor.
    mv_block_type = iv_block_type.
    mt_in         = it_in.
  ENDMETHOD.

  METHOD parse.

    DATA(lv_block_type) = io_body->shift( 1 ).
    DATA(lo_parser) = NEW zcl_wasm_parser( ).

    lo_parser->parse_instructions(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_in) ).

    ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

    ri_instruction = NEW zcl_wasm_loop(
      iv_block_type = lv_block_type
      it_in         = lt_in ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_loop' ).
  ENDMETHOD.

ENDCLASS.
