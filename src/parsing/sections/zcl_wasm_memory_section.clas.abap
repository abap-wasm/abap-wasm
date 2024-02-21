CLASS zcl_wasm_memory_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_memory_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-memsec

    DATA(lv_count) = io_body->shift_u32( ).
    " WRITE: / 'memories:', lv_count.

    IF lv_count > 1.
      RAISE EXCEPTION NEW zcx_wasm( text = |multiple memories| ).
    ENDIF.

    DO lv_count TIMES.
      DATA(lv_limit) = io_body->shift( 1 ).

      CASE lv_limit.
        WHEN '00'.
          DATA(lv_min) = io_body->shift_u32( ).
          DATA(lv_max) = lv_min.
        WHEN '01'.
          lv_min = io_body->shift_u32( ).
          lv_max = io_body->shift_u32( ).
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |parse_memory: todo| ).
      ENDCASE.

      IF lv_max < lv_min.
        RAISE EXCEPTION NEW zcx_wasm( text = |size minimum must not be greater than maximum| ).
      ELSEIF lv_max > zcl_wasm_memory=>c_max_pages.
        RAISE EXCEPTION NEW zcx_wasm( text = |memory size must be at most { zcl_wasm_memory=>c_max_pages } pages (4GiB)| ).
      ENDIF.

* todo

    ENDDO.

  ENDMETHOD.

ENDCLASS.
