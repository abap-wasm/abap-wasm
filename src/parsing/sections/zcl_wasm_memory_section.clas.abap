CLASS zcl_wasm_memory_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_memory) TYPE REF TO zcl_wasm_memory_section
      RAISING
        zcx_wasm.

    METHODS constructor
      IMPORTING
        iv_min TYPE int8 OPTIONAL
        iv_max TYPE int8 OPTIONAL.

    METHODS instantiate
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

  PRIVATE SECTION.
    DATA mv_min TYPE int8.
    DATA mv_max TYPE int8.
ENDCLASS.

CLASS zcl_wasm_memory_section IMPLEMENTATION.

  METHOD constructor.
    mv_min = iv_min.
    mv_max = iv_max.
  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-memsec

    DATA(lv_count) = io_body->shift_u32( ).
    " WRITE: / 'memories:', lv_count.

    IF lv_count = 0.
      RETURN.
    ELSEIF lv_count > 1.
      RAISE EXCEPTION NEW zcx_wasm( text = |multiple memories| ).
    ELSEIF lv_count <> 1.
      RAISE EXCEPTION NEW zcx_wasm( text = |not just one memory| ).
    ENDIF.

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
    ELSEIF lv_max > zif_wasm_memory_linear=>c_max_pages.
      RAISE EXCEPTION NEW zcx_wasm( text = |memory size must be at most { zif_wasm_memory_linear=>c_max_pages } pages (4GiB)| ).
    ENDIF.

    ro_memory = NEW #(
      iv_min = lv_min
      iv_max = lv_max ).

  ENDMETHOD.

  METHOD instantiate.

    io_memory->set_linear( NEW zcl_wasm_memory_linear( ) ).

  ENDMETHOD.

ENDCLASS.
