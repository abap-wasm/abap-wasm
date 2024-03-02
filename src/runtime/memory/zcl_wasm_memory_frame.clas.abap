CLASS zcl_wasm_memory_frame DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_memory_frame.
  PRIVATE SECTION.
    DATA mt_locals TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_wasm_memory_frame IMPLEMENTATION.

  METHOD zif_wasm_memory_frame~local_get.

    DATA(lv_index) = iv_index + 1.
    READ TABLE mt_locals INDEX lv_index INTO ri_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_frame: not found in local memory, local_get'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wasm_memory_frame~local_push_first.

    INSERT ii_value INTO mt_locals INDEX 1.

  ENDMETHOD.

  METHOD zif_wasm_memory_frame~local_push_last.

    INSERT ii_value INTO TABLE mt_locals.

  ENDMETHOD.


  METHOD zif_wasm_memory_frame~local_set.

    DATA(lv_index) = iv_index + 1.
    MODIFY mt_locals INDEX lv_index FROM ii_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory_frame: not found in local memory, local_set'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
