INTERFACE zif_wasm_value PUBLIC.

  TYPES: ty_values TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.

  METHODS get_type
    RETURNING VALUE(rv_type) TYPE zcl_wasm_types=>ty_type.

ENDINTERFACE.
