CLASS zcl_wasm_memory_linear DEFINITION PUBLIC.
  PUBLIC SECTION.
    CONSTANTS c_page_size TYPE i VALUE 65536.
    CONSTANTS c_max_pages TYPE i VALUE 65536.

    METHODS constructor
      IMPORTING
        iv_max TYPE int8
        iv_min TYPE int8
      RAISING
        zcx_wasm.

    METHODS set
      IMPORTING
        iv_offset TYPE int8 OPTIONAL
        iv_bytes  TYPE xsequence
      RAISING
        zcx_wasm.

    METHODS get
      IMPORTING
        iv_length       TYPE int8 OPTIONAL
        iv_offset       TYPE int8 OPTIONAL
        iv_align        TYPE int8 OPTIONAL
      RETURNING
        VALUE(rv_bytes) TYPE xstring
      RAISING
        zcx_wasm.

    METHODS grow
      IMPORTING
        iv_pages TYPE int8
      RAISING
        zcx_wasm.

    METHODS size_in_pages
      RETURNING
        VALUE(rv_pages) TYPE i
      RAISING
        zcx_wasm.

    METHODS size_in_bytes
      RETURNING
        VALUE(rv_bytes) TYPE int8
      RAISING
        zcx_wasm.
  PRIVATE SECTION.
    TYPES ty_page TYPE x LENGTH c_page_size.

    CLASS-DATA gv_page TYPE REF TO ty_page.
    DATA mt_pages TYPE STANDARD TABLE OF ty_page WITH DEFAULT KEY.
    DATA mv_max TYPE int8.
    DATA mv_min TYPE int8.
ENDCLASS.

CLASS zcl_wasm_memory_linear IMPLEMENTATION.

  METHOD constructor.
    mv_min = iv_min.
    mv_max = iv_max.

    grow( mv_min ).
  ENDMETHOD.

  METHOD grow.
    "##feature-start=debug
    IF iv_pages < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: linear_grow, negative pages'.
    ELSEIF size_in_pages( ) + iv_pages >= c_max_pages.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: linear_grow, max pages reached'.
    ELSEIF iv_pages >= 1000.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: todo, its too slow, and will crash node anyhow'.
    ENDIF.
    "##feature-end=debug

    DO iv_pages TIMES.
      INSERT INITIAL LINE INTO TABLE mt_pages.
    ENDDO.
  ENDMETHOD.

  METHOD size_in_pages.
    rv_pages = lines( mt_pages ).
  ENDMETHOD.

  METHOD size_in_bytes.
    rv_bytes = lines( mt_pages ) * c_page_size.
  ENDMETHOD.

  METHOD set.

    DATA(lv_length) = xstrlen( iv_bytes ).

    "##feature-start=debug
    IF iv_offset + lv_length > lines( mt_pages ) * c_page_size.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |linear_set: out of bounds, { iv_offset }, { lv_length }|.
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'linear_set: offset is negative'.
    ENDIF.
    "##feature-end=debug

    IF lv_length = 0.
      RETURN.
    ENDIF.

    DATA(lv_page) = iv_offset DIV c_page_size.
    lv_page = lv_page + 1.
    READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page ##SUBRC_OK.

    DATA(lv_offset) = iv_offset MOD c_page_size.

    IF lv_offset + lv_length <= c_page_size.
      gv_page->*+lv_offset(lv_length) = iv_bytes.
    ELSE.
      DATA(lv_written) = c_page_size - lv_offset.
      gv_page->*+lv_offset(lv_written) = iv_bytes(lv_written).

      WHILE lv_written < lv_length.
        lv_page = lv_page + 1.
        READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page ##SUBRC_OK.
        lv_length = lv_length - lv_written.
        lv_length = nmin(
          val1 = lv_length
          val2 = c_page_size ).
        gv_page->*(lv_length) = iv_bytes+lv_written(lv_length).
        lv_written = lv_written + lv_length.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.

  METHOD get.
* https://rsms.me/wasm-intro#addressing-memory

* todo: refactor this to CHANGING as the caller always knows the length, then allocating an extra
* value is not required, plus the CONCATENATE can be replaced with direct writing into the CHANGING
    DATA lv_byte TYPE x LENGTH 1.

    "##feature-start=debug
    IF iv_length <= 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'linear_get: negative or zero length'.
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'linear_get: negative offset'.
    ENDIF.
    "##feature-end=debug

    DATA(lv_page) = iv_offset DIV c_page_size.
    lv_page = lv_page + 1.
    READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page.
    "##feature-start=debug
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |linear_get: out of bounds, getting page { lv_page }, { iv_offset }|.
    ENDIF.
    "##feature-end=debug

    DATA(lv_offset) = iv_offset MOD c_page_size.

    IF lv_offset + iv_length <= c_page_size.
      rv_bytes = gv_page->*+lv_offset(iv_length).
      rv_bytes = zcl_wasm_binary_stream=>reverse_hex( rv_bytes ).
    ELSE.
* return multiple bytes in endian order
      DO iv_length TIMES.
        IF lv_offset = c_page_size.
          lv_page = lv_page + 1.
          READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page.
          "##feature-start=debug
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_wasm
              EXPORTING
                text = |linear_get: out of bounds, getting page { lv_page }|.
          ENDIF.
          "##feature-end=debug
          lv_offset = 0.
        ENDIF.

        lv_byte = gv_page->*+lv_offset(1).
        CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
        lv_offset = lv_offset + 1.
      ENDDO.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
