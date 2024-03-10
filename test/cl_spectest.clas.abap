CLASS cl_spectest DEFINITION PUBLIC.
  PUBLIC SECTION.
* https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#spectest-host-module
    INTERFACES zif_wasm_module.
ENDCLASS.

CLASS cl_spectest IMPLEMENTATION.

  METHOD zif_wasm_module~execute_function_export.
* for now, do nothing,
    RETURN.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_spectest: get_export_by_name'.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_spectest: instantiate'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_spectest: get_memory'.
  ENDMETHOD.

ENDCLASS.
