CLASS zcl_wasm_global_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_global_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-globalsec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_type) = io_body->shift( 1 ).
      DATA(lv_mut) = io_body->shift( 1 ).

      " WRITE: / 'type:', lv_type.
      " WRITE: / 'mut:', lv_mut.

      zcl_wasm_parser=>parse_instructions(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = DATA(lv_last_opcode)
          et_instructions = DATA(lt_instructions) ).
      IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
        RAISE EXCEPTION NEW zcx_wasm( text = |parse_global, expected end| ).
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
