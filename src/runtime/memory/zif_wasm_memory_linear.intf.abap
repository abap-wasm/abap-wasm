INTERFACE zif_wasm_memory_linear PUBLIC.

  CONSTANTS c_page_size TYPE i VALUE 65536.
  CONSTANTS c_max_pages TYPE i VALUE 65536.

  METHODS set
    IMPORTING
      iv_offset TYPE int8 OPTIONAL
      iv_bytes  TYPE xsequence
    RAISING
      zcx_wasm.

  METHODS get
    IMPORTING
      iv_length       TYPE int8 OPTIONAL
      iv_offset       TYPE int8 OPTIONAL
      iv_align        TYPE int8 OPTIONAL
    RETURNING
      VALUE(rv_bytes) TYPE xstring
    RAISING
      zcx_wasm.

  METHODS grow
    IMPORTING
      iv_pages TYPE int8
    RAISING
      zcx_wasm.

  METHODS size_in_pages
    RETURNING
      VALUE(rv_pages) TYPE int8
    RAISING
      zcx_wasm.

  METHODS size_in_bytes
    RETURNING
      VALUE(rv_bytes) TYPE int8
    RAISING
      zcx_wasm.

ENDINTERFACE.
