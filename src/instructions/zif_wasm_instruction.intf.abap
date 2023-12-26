INTERFACE zif_wasm_instruction PUBLIC.

  METHODS execute
    IMPORTING
      io_memory TYPE REF TO zcl_wasm_memory.

* to_string( ) TYPE string
* to_xstring( ) TYPE xstring

ENDINTERFACE.