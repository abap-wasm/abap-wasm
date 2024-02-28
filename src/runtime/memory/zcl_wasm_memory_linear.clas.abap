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
    CLASS-DATA gv_empty_page TYPE xstring.

    DATA mv_linear TYPE xstring.
    DATA mv_max TYPE int8.
    DATA mv_min TYPE int8.
ENDCLASS.

CLASS zcl_wasm_memory_linear IMPLEMENTATION.

  METHOD constructor.
    DATA lv_hex32 TYPE x LENGTH 32.

    mv_min = iv_min.
    mv_max = iv_max.

    IF gv_empty_page IS INITIAL.
      lv_hex32 = '0000000000000000000000000000000000000000000000000000000000000000'.
      DO 2048 TIMES.
        CONCATENATE gv_empty_page lv_hex32 INTO gv_empty_page IN BYTE MODE.
      ENDDO.
      ASSERT xstrlen( gv_empty_page ) = zif_wasm_memory_linear=>c_page_size.
    ENDIF.

    zif_wasm_memory_linear~grow( mv_min ).
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~grow.
    IF iv_pages < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: linear_grow, negative pages' ).
    ENDIF.

    IF zif_wasm_memory_linear~size_in_pages( ) + iv_pages >= zif_wasm_memory_linear=>c_max_pages.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: linear_grow, max pages reached' ).
    ENDIF.

    IF iv_pages >= 1000.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: todo, its too slow, and will crash node anyhow' ).
    ENDIF.

    DO iv_pages TIMES.
      CONCATENATE mv_linear gv_empty_page INTO mv_linear IN BYTE MODE.
    ENDDO.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~size_in_pages.
    rv_pages = xstrlen( mv_linear ) / zif_wasm_memory_linear=>c_page_size.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~size_in_bytes.
    rv_bytes = xstrlen( mv_linear ).
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~set.
    DATA(lv_length) = xstrlen( iv_bytes ).

    IF iv_offset = 0.
      CONCATENATE iv_bytes mv_linear+lv_length INTO mv_linear IN BYTE MODE.
    ELSE.
      lv_length = lv_length + iv_offset.
      CONCATENATE mv_linear(iv_offset) iv_bytes mv_linear+lv_length INTO mv_linear IN BYTE MODE.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~get.
* https://rsms.me/wasm-intro#addressing-memory

    DATA lv_byte TYPE x LENGTH 1.

    IF iv_length = 0 AND iv_offset = 0.
* todo, does this respect endians?
      rv_bytes = mv_linear.
      RETURN.
    ENDIF.

    DATA(lv_length) = xstrlen( mv_linear ).

    IF iv_offset + iv_length > lv_length.
      RAISE EXCEPTION NEW zcx_wasm( text = 'linear_get: out of bounds' ).
    ELSEIF iv_length <= 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'linear_get: negative or zero length' ).
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'linear_get: negative offset' ).
    ENDIF.

* return multiple bytes in endian order
    DATA(lv_offset) = iv_offset.
    DO iv_length TIMES.
      lv_byte = mv_linear+lv_offset(1).
      CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.

  METHOD zif_wasm_memory_linear~get_raw.
* https://rsms.me/wasm-intro#addressing-memory

    IF iv_length = 0 AND iv_offset = 0.
      rv_bytes = mv_linear.
      RETURN.
    ENDIF.

    DATA(lv_length) = xstrlen( mv_linear ).

    IF iv_offset + iv_length > lv_length.
      RAISE EXCEPTION NEW zcx_wasm( text = 'linear_get: out of bounds' ).
    ELSEIF iv_length <= 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'linear_get: negative or zero length' ).
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'linear_get: negative offset' ).
    ENDIF.

    rv_bytes = mv_linear+iv_offset(iv_length).

  ENDMETHOD.

ENDCLASS.
