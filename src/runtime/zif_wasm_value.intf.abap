INTERFACE zif_wasm_value PUBLIC.

  TYPES ty_values TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.

  TYPES ty_hex TYPE x LENGTH 1.

  METHODS get_type
    RETURNING
      VALUE(rv_type) TYPE ty_hex.

* for debugging
  METHODS human_readable_value
    RETURNING
      VALUE(rv_string) TYPE string.

ENDINTERFACE.
