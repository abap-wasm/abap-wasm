INTERFACE zif_wasm_module PUBLIC.

  TYPES:
      BEGIN OF ty_export,
        name  TYPE string,
        type  TYPE x LENGTH 1,
        index TYPE int8,
      END OF ty_export .
  TYPES ty_exports TYPE HASHED TABLE OF ty_export WITH UNIQUE KEY name .

  METHODS execute_function_export
    IMPORTING
      !iv_name          TYPE string
      !it_parameters    TYPE zif_wasm_value=>ty_values OPTIONAL
    RETURNING
      VALUE(rt_results) TYPE zif_wasm_value=>ty_values
    RAISING
      zcx_wasm.

  METHODS get_export_by_name
    IMPORTING
      !iv_name         TYPE string
    RETURNING
      VALUE(rs_export) TYPE ty_export
    RAISING
      zcx_wasm.

* not mandatory to call, must be called max once before execute_function_export
  METHODS instantiate
    RETURNING
      VALUE(ri_module) TYPE REF TO zif_wasm_module
    RAISING
      zcx_wasm.

  METHODS get_memory
    RETURNING
      VALUE(ro_memory) TYPE REF TO zcl_wasm_memory
    RAISING
      zcx_wasm.

ENDINTERFACE.
