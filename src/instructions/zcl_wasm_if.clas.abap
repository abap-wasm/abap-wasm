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
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
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

    zcl_wasm_parser=>parse_instructions(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_in1) ).

    IF lv_last_opcode = zif_wasm_opcodes=>c_opcodes-else_.
      zcl_wasm_parser=>parse_instructions(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = lv_last_opcode
          et_instructions = DATA(lt_in2) ).
    ENDIF.

    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION NEW zcx_wasm( text = |if parse: expected end| ).
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

* todo, more regarding block type?

* If c is non-zero, then enter
    DATA(lv_value) = io_memory->stack_pop_i32( )->get_signed( ).
    IF lv_value <> 0.
      LOOP AT mt_in1 INTO DATA(lo_instruction).
        rv_control = lo_instruction->execute(
          io_memory = io_memory
          io_module = io_module ).
        IF rv_control = zif_wasm_instruction=>c_control-return_.
          RETURN.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT mt_in2 INTO lo_instruction.
        rv_control = lo_instruction->execute(
          io_memory = io_memory
          io_module = io_module ).
        IF rv_control = zif_wasm_instruction=>c_control-return_.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
