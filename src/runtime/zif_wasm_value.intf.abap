INTERFACE zif_wasm_value PUBLIC.

  METHODS get_type
    RETURNING VALUE(rv_type) TYPE zcl_wasm_value_types=>ty_value_type.

ENDINTERFACE.
