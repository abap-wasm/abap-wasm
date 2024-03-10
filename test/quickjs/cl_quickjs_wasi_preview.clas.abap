CLASS cl_quickjs_wasi_preview DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
ENDCLASS.

CLASS cl_quickjs_wasi_preview IMPLEMENTATION.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_wasi_preview: execute_function_export'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    " fd_close
    " fd_read
    " fd_write
    " proc_exit
    " fd_seek

    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_wasi_preview: get_export_by_name'.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_wasi_preview: instantiate'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_wasi_preview: get_memory'.
  ENDMETHOD.

ENDCLASS.
