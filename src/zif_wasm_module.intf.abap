INTERFACE zif_wasm_module PUBLIC.

  METHODS execute_function_export
    IMPORTING
      !iv_name          TYPE string
      !it_parameters    TYPE zif_wasm_value=>ty_values OPTIONAL
    RETURNING
      VALUE(rt_results) TYPE zif_wasm_value=>ty_values
    RAISING
      zcx_wasm.

* not mandatory to call, must be called max once before execute_function_export
  METHODS instantiate
    RAISING
      zcx_wasm.

  METHODS get_memory
    RETURNING
      VALUE(ro_memory) TYPE REF TO zcl_wasm_memory
    RAISING
      zcx_wasm.

ENDINTERFACE.
