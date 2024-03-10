CLASS cl_wbindgen_placeholder DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
ENDCLASS.

CLASS cl_wbindgen_placeholder IMPLEMENTATION.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_wasi_preview: execute_function_export'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    " __wbg_error_f851667af71bcfc6
    " __wbg_new_abda76e883ba8a5f
    " __wbg_stack_658279fe44541cf6
    " __wbindgen_object_drop_ref
    " __wbindgen_throw

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
