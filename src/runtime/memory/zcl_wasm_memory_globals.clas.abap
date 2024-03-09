CLASS zcl_wasm_memory_globals DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_memory_globals.
  PRIVATE SECTION.
    DATA mt_globals TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_wasm_memory_globals IMPLEMENTATION.

  METHOD zif_wasm_memory_globals~get.
    READ TABLE mt_globals INDEX iv_index + 1 INTO rv_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_memory: global_get, not found, index { iv_index }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_memory_globals~set.
* todo: raise error if the set changes the type of the global?
    IF lines( mt_globals ) < iv_index + 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: global_set, not found'.
    ENDIF.
    mt_globals[ iv_index + 1 ] = ii_value.
  ENDMETHOD.

  METHOD zif_wasm_memory_globals~append.
    APPEND ii_value TO mt_globals.
    rv_index = lines( mt_globals ) - 1.
  ENDMETHOD.

ENDCLASS.
