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

    NEW zcl_wasm_parser( )->parse_instructions(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_instructions) ).

    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION NEW zcx_wasm( text = |block, expected end| ).
    ENDIF.

    ri_instruction = NEW zcl_wasm_block(
      iv_block_type   = lv_block_type
      it_instructions = lt_instructions ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* todo: label and block type?
    NEW zcl_wasm_vm(
      io_memory = io_memory
      io_module = io_module )->execute( mt_instructions ).
  ENDMETHOD.

ENDCLASS.
