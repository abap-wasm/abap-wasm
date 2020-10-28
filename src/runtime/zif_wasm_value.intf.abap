INTERFACE zif_wasm_value PUBLIC.

  METHODS get_type
    RETURNING VALUE(rv_type) TYPE zcl_wasm_types=>ty_type.

ENDINTERFACE.
