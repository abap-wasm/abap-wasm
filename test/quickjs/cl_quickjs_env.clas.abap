CLASS cl_quickjs_env DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.
ENDCLASS.

CLASS cl_quickjs_env IMPLEMENTATION.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_env: execute_function_export'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        textid = 'cl_quickjs_env: get_export_by_name'.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    mo_memory = NEW zcl_wasm_memory( ).
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.

ENDCLASS.
