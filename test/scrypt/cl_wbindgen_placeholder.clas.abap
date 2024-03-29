CLASS cl_wbindgen_placeholder DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mt_functions TYPE STANDARD TABLE OF string WITH EMPTY KEY.
ENDCLASS.

CLASS cl_wbindgen_placeholder IMPLEMENTATION.

  METHOD constructor.
    INSERT |__wbg_error_f851667af71bcfc6| INTO TABLE mt_functions.
    INSERT |__wbg_new_abda76e883ba8a5f| INTO TABLE mt_functions.
    INSERT |__wbg_stack_658279fe44541cf6| INTO TABLE mt_functions.
    INSERT |__wbindgen_object_drop_ref| INTO TABLE mt_functions.
    INSERT |__wbindgen_throw| INTO TABLE mt_functions.
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_wbindgen_placeholder: execute_function_export'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    READ TABLE mt_functions WITH KEY table_line = iv_name INTO DATA(lv_name).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |cl_wbindgen_placeholder: get_export_by_name { iv_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    ri_module ?= me.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_wbindgen_placeholder: get_memory'.
  ENDMETHOD.

ENDCLASS.
