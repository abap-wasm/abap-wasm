CLASS zcl_wasm_element_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_element_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-elemsec

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
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          DO io_body->shift_u32( ) TIMES.
            DATA(lv_funcidx) = io_body->shift_u32( ).
            " WRITE: / 'funcidx', lv_funcidx.
          ENDDO.
        WHEN 1.
          DATA(lv_elemkind) = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            lv_funcidx = io_body->shift_u32( ).
            " WRITE: / 'funcidx', lv_funcidx.
          ENDDO.
        WHEN 2.
          DATA(lv_tableidx) = io_body->shift_u32( ).

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          lv_elemkind = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            lv_funcidx = io_body->shift_u32( ).
            " WRITE: / 'funcidx', lv_funcidx.
          ENDDO.
        WHEN 3.
          lv_elemkind = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            lv_funcidx = io_body->shift_u32( ).
          ENDDO.
        WHEN 4.
          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          DO io_body->shift_u32( ) TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN 5.
          DATA(lv_reftype) = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN 6.
          lv_tableidx = io_body->shift_u32( ).

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          lv_reftype = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN 7.
          lv_reftype = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |elementtype: { lv_type }| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
