INTERFACE zif_wasm_memory_linear PUBLIC.

  CONSTANTS c_page_size TYPE i VALUE 65536.
  CONSTANTS c_max_pages TYPE i VALUE 65536.

* todo, rename method
  METHODS linear_set
    IMPORTING
      iv_offset TYPE int8 OPTIONAL
      iv_bytes  TYPE xstring
    RAISING
      zcx_wasm.

* todo, rename method
  METHODS linear_get
    IMPORTING
      iv_length       TYPE int8 OPTIONAL
      iv_offset       TYPE int8 OPTIONAL
      iv_align        TYPE int8 OPTIONAL
    RETURNING
      VALUE(rv_bytes) TYPE xstring
    RAISING
      zcx_wasm.

ENDINTERFACE.
