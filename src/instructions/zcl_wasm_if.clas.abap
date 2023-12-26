CLASS zcl_wasm_if DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type TYPE xstring
        it_in1        TYPE zif_wasm_instruction=>ty_list
        it_in2        TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_block_type TYPE xstring.
ENDCLASS.

CLASS zcl_wasm_if IMPLEMENTATION.

  METHOD constructor.
    mv_block_type = iv_block_type.
  ENDMETHOD.

  METHOD parse.

    DATA(lv_block_type) = io_body->shift( 1 ).
    DATA(lo_parser) = NEW zcl_wasm_parser( ).

    lo_parser->parse_instructions(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_in1) ).

    IF lv_last_opcode = zif_wasm_opcodes=>c_opcodes-else_.
      lo_parser->parse_instructions(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = lv_last_opcode
          et_instructions = DATA(lt_in2) ).
    ENDIF.

    ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

    ri_instruction = NEW zcl_wasm_if(
      iv_block_type = lv_block_type
      it_in1        = lt_in1
      it_in2        = lt_in2 ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.