CLASS zcl_wasm_table_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_table_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-tablesec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_reftype) = io_body->shift( 1 ).

      CASE lv_reftype.
        WHEN '70' OR '6F'.
          DATA(lv_limit) = io_body->shift( 1 ).

          CASE lv_limit.
            WHEN '00'.
              DATA(lv_min) = io_body->shift_u32( ).
              DATA(lv_max) = 0.
            WHEN '01'.
              lv_min = io_body->shift_u32( ).
              lv_max = io_body->shift_u32( ).
            WHEN OTHERS.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_table: todo| ).
          ENDCASE.
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |parse_table: todo| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
