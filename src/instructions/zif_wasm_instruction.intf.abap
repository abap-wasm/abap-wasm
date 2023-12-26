INTERFACE zif_wasm_instruction PUBLIC.

  TYPES ty_list TYPE STANDARD TABLE OF REF TO zif_wasm_instruction WITH DEFAULT KEY.

  METHODS execute
    IMPORTING
      io_memory TYPE REF TO zcl_wasm_memory
      io_module TYPE REF TO zcl_wasm_module.

* to_string( ) TYPE string
* to_xstring( ) TYPE xstring

ENDINTERFACE.