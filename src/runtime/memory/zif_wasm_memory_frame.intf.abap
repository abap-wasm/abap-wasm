INTERFACE zif_wasm_memory_frame PUBLIC.

  METHODS local_push_first
    IMPORTING
      !ii_value TYPE REF TO zif_wasm_value.

  METHODS local_push_last
    IMPORTING
      !ii_value TYPE REF TO zif_wasm_value.

  METHODS local_get
    IMPORTING
      !iv_index       TYPE int8
    RETURNING
      VALUE(ri_value) TYPE REF TO zif_wasm_value
    RAISING
      zcx_wasm.

  METHODS local_set
    IMPORTING
      iv_index TYPE int8
      ii_value TYPE REF TO zif_wasm_value
    RAISING
      zcx_wasm.

ENDINTERFACE.
