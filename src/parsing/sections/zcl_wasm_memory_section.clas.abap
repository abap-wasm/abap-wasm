CLASS zcl_wasm_memory_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body         TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_memory) TYPE REF TO zcl_wasm_memory_section
      RAISING
        zcx_wasm.

    METHODS constructor
      IMPORTING
        iv_has_memory TYPE abap_bool OPTIONAL
        iv_min        TYPE int8 OPTIONAL
        iv_max        TYPE int8 OPTIONAL.

    METHODS instantiate
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

  PRIVATE SECTION.
    DATA mv_min TYPE int8.
    DATA mv_max TYPE int8.
    DATA mv_has_memory TYPE abap_bool.
ENDCLASS.

CLASS zcl_wasm_memory_section IMPLEMENTATION.

  METHOD constructor.
    mv_min = iv_min.
    mv_max = iv_max.
    mv_has_memory = iv_has_memory.
  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-memsec

    DATA(lv_count) = io_body->shift_u32( ).
    " WRITE: / 'memories:', lv_count.

    IF lv_count = 0.
      RETURN.
    ELSEIF lv_count > 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |multiple memories|.
    ELSEIF lv_count <> 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |not just one memory|.
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
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_memory: todo|.
    ENDCASE.

    IF lv_max < lv_min.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |size minimum must not be greater than maximum|.
    ELSEIF lv_max > zcl_wasm_memory_linear=>c_max_pages.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |memory size must be at most { zcl_wasm_memory_linear=>c_max_pages } pages (4GiB)|.
    ENDIF.

    ro_memory = NEW #(
      iv_has_memory = abap_true
      iv_min        = lv_min
      iv_max        = lv_max ).

  ENDMETHOD.

  METHOD instantiate.

    IF mv_has_memory = abap_true.
      DATA(lo_linear) = NEW zcl_wasm_memory_linear(
        iv_min = mv_min
        iv_max = mv_max ).

      IF io_memory->mi_linear IS NOT INITIAL.
* it can be imported
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |memory section: memory already instantiated|.
      ENDIF.

      io_memory->mi_linear = lo_linear.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
