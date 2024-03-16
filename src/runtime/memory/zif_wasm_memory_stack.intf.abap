INTERFACE zif_wasm_memory_stack PUBLIC.

  METHODS push
    IMPORTING
      !ii_value TYPE REF TO zif_wasm_value .

  METHODS pop
    RETURNING
      VALUE(ri_value) TYPE REF TO zif_wasm_value
    RAISING
      zcx_wasm.

  METHODS peek
    RETURNING
      VALUE(ri_value) TYPE REF TO zif_wasm_value
    RAISING
      zcx_wasm.

  METHODS pop_i32
    RETURNING
      VALUE(ro_value) TYPE REF TO zcl_wasm_i32
    RAISING
      zcx_wasm.

  METHODS pop_i64
    RETURNING
      VALUE(ro_value) TYPE REF TO zcl_wasm_i64
    RAISING
      zcx_wasm.

  METHODS get_length
    RETURNING
      VALUE(rv_length) TYPE i .

ENDINTERFACE.
