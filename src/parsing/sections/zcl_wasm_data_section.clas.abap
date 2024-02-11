CLASS zcl_wasm_data_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_data_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-datasec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_type) = io_body->shift_u32( ).

      CASE lv_type.
        WHEN 0.
          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = DATA(lv_last_opcode)
              et_instructions = DATA(lt_instructions) ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          DATA(lv_vec) = io_body->shift_u32( ).
          DATA(lv_contents) = io_body->shift( lv_vec ).
        WHEN 1.
          lv_vec = io_body->shift_u32( ).
          lv_contents = io_body->shift( lv_vec ).
        WHEN 2.
          DATA(lv_memidx) = io_body->shift_u32( ).

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          lv_vec = io_body->shift_u32( ).
          lv_contents = io_body->shift( lv_vec ).
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |parse_data, type: { lv_type }| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
