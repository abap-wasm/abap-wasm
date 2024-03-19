CLASS zcl_wasm_memory_linear DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_memory_linear.

    METHODS constructor
      IMPORTING
        iv_max TYPE int8
        iv_min TYPE int8
      RAISING
        zcx_wasm.
  PRIVATE SECTION.
    TYPES ty_page TYPE x LENGTH zif_wasm_memory_linear=>c_page_size.

    CLASS-DATA gv_page TYPE REF TO ty_page.
    DATA mt_pages TYPE STANDARD TABLE OF ty_page WITH DEFAULT KEY.
    DATA mv_max TYPE int8.
    DATA mv_min TYPE int8.
ENDCLASS.

CLASS zcl_wasm_memory_linear IMPLEMENTATION.

  METHOD constructor.
    mv_min = iv_min.
    mv_max = iv_max.

    zif_wasm_memory_linear~grow( mv_min ).
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~grow.
    IF iv_pages < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: linear_grow, negative pages'.
    ELSEIF zif_wasm_memory_linear~size_in_pages( ) + iv_pages >= zif_wasm_memory_linear=>c_max_pages.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: linear_grow, max pages reached'.
    ELSEIF iv_pages >= 1000.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: todo, its too slow, and will crash node anyhow'.
    ENDIF.

    DO iv_pages TIMES.
      INSERT INITIAL LINE INTO TABLE mt_pages.
    ENDDO.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~size_in_pages.
    rv_pages = lines( mt_pages ).
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~size_in_bytes.
    rv_bytes = lines( mt_pages ) * zif_wasm_memory_linear=>c_page_size.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~set.

    DATA(lv_length) = xstrlen( iv_bytes ).
    IF iv_offset + lv_length > lines( mt_pages ) * zif_wasm_memory_linear=>c_page_size.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |linear_set: out of bounds, { iv_offset }, { lv_length }|.
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'linear_set: offset is negative'.
    ELSEIF lv_length = 0.
      RETURN.
    ENDIF.

    DATA(lv_page) = iv_offset DIV zif_wasm_memory_linear=>c_page_size.
    lv_page = lv_page + 1.
    READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page ##SUBRC_OK.

    DATA(lv_offset) = iv_offset MOD zif_wasm_memory_linear=>c_page_size.

    IF lv_offset + lv_length <= zif_wasm_memory_linear=>c_page_size.
      gv_page->*+lv_offset(lv_length) = iv_bytes.
    ELSE.
      DATA(lv_written) = zif_wasm_memory_linear=>c_page_size - lv_offset.
      gv_page->*+lv_offset(lv_written) = iv_bytes(lv_written).

      WHILE lv_written < lv_length.
        lv_page = lv_page + 1.
        READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page ##SUBRC_OK.
        lv_length = lv_length - lv_written.
        lv_length = nmin(
          val1 = lv_length
          val2 = zif_wasm_memory_linear=>c_page_size ).
        gv_page->*(lv_length) = iv_bytes+lv_written(lv_length).
        lv_written = lv_written + lv_length.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~get.
* https://rsms.me/wasm-intro#addressing-memory

* todo: refactor this to CHANGING as the caller always knows the length, then allocating an extra
* value is not required, plus the CONCATENATE can be replaced with direct writing into the CHANGING
    DATA lv_byte TYPE x LENGTH 1.

    IF iv_length <= 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'linear_get: negative or zero length'.
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'linear_get: negative offset'.
    ENDIF.

    DATA(lv_page) = iv_offset DIV zif_wasm_memory_linear=>c_page_size.
    lv_page = lv_page + 1.
    READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |linear_get: out of bounds, getting page { lv_page }, { iv_offset }|.
    ENDIF.

    DATA(lv_offset) = iv_offset MOD zif_wasm_memory_linear=>c_page_size.

* return multiple bytes in endian order
    DO iv_length TIMES.
      IF lv_offset = zif_wasm_memory_linear=>c_page_size.
        lv_page = lv_page + 1.
        READ TABLE mt_pages INDEX lv_page REFERENCE INTO gv_page.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_wasm
            EXPORTING
              text = |linear_get: out of bounds, getting page { lv_page }|.
        ENDIF.
        lv_offset = 0.
      ENDIF.

      lv_byte = gv_page->*+lv_offset(1).
      CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
