CLASS zcl_wasm_memory_linear DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_memory_linear.
  PRIVATE SECTION.
    DATA mv_linear TYPE xstring.
ENDCLASS.

CLASS zcl_wasm_memory_linear IMPLEMENTATION.

  METHOD zif_wasm_memory_linear~set.
    IF iv_offset <> 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: linear_set, offset todo' ).
    ENDIF.

    mv_linear = iv_bytes.
  ENDMETHOD.

  METHOD zif_wasm_memory_linear~get.
* https://rsms.me/wasm-intro#addressing-memory

* alignment values:
* 0 = 8-bit, 1 = 16-bit, 2 = 32-bit, and 3 = 64-bit

    DATA lv_byte TYPE x LENGTH 1.

    IF iv_length = 0 AND iv_offset = 0.
      rv_bytes = mv_linear.
      RETURN.
    ENDIF.

    DATA(lv_i) = xstrlen( mv_linear ).

    IF lv_i < iv_offset.
* it allocates 64k bytes pages at a time?
* hmm,      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: linear_get, out of bounds' ).
      rv_bytes = '00'.
      RETURN.
    ELSEIF iv_length <= 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: linear_get, negative or zero length' ).
    ELSEIF iv_offset < 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: linear_get, negative offset' ).
    ENDIF.

* return multiple bytes in endian order
    DATA(lv_offset) = iv_offset.
    DO iv_length TIMES.
      IF lv_offset < lv_i.
        lv_byte = mv_linear+lv_offset(1).
      ELSE.
        lv_byte = '00'.
      ENDIF.

      CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
