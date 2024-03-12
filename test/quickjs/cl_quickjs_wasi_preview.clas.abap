CLASS cl_quickjs_wasi_preview DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mt_functions TYPE STANDARD TABLE OF string WITH EMPTY KEY.
ENDCLASS.

CLASS cl_quickjs_wasi_preview IMPLEMENTATION.

  METHOD constructor.
    INSERT 'fd_close' INTO TABLE mt_functions.
    INSERT 'fd_read' INTO TABLE mt_functions.
    INSERT 'fd_write' INTO TABLE mt_functions.
    INSERT 'proc_exit' INTO TABLE mt_functions.
    INSERT 'fd_seek' INTO TABLE mt_functions.
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_quickjs_wasi_preview: execute_function_export'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    READ TABLE mt_functions WITH KEY table_line = iv_name INTO DATA(lv_name).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |cl_quickjs_wasi_preview: get_export_by_name "{ iv_name }"|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    ri_module ?= me.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_quickjs_wasi_preview: get_memory'.
  ENDMETHOD.

ENDCLASS.
