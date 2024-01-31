INTERFACE zif_wasm PUBLIC.

  TYPES: BEGIN OF ty_name_and_parameter,
           name       TYPE string,
           parameters TYPE xstring,
         END OF ty_name_and_parameter.
  TYPES ty_name_and_parameters TYPE STANDARD TABLE OF ty_name_and_parameter WITH DEFAULT KEY.

  METHODS execute_function_export
    IMPORTING
      !iv_name          TYPE string
      !it_parameters    TYPE zif_wasm_value=>ty_values OPTIONAL
    RETURNING
      VALUE(rt_results) TYPE zif_wasm_value=>ty_values
    RAISING
      zcx_wasm.

  METHODS list_function_exports
    RETURNING
      VALUE(rt_functions) TYPE ty_name_and_parameters
    RAISING
      zcx_wasm.

ENDINTERFACE.
