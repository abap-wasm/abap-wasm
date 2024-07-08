INTERFACE zif_wasm_memory_globals PUBLIC.

  METHODS get
    IMPORTING
      iv_index        TYPE int8
    RETURNING
      VALUE(rv_value) TYPE REF TO zif_wasm_value
    RAISING
      zcx_wasm.

  METHODS set
    IMPORTING
      iv_index TYPE int8
      ii_value TYPE REF TO zif_wasm_value
    RAISING
      zcx_wasm.

  METHODS append
    IMPORTING
      ii_value        TYPE REF TO zif_wasm_value
    RETURNING
      VALUE(rv_index) TYPE i.

ENDINTERFACE.
