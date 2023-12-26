INTERFACE zif_wasm_instruction PUBLIC.

  METHODS execute
    IMPORTING
      io_memory TYPE REF TO zcl_wasm_memory.

ENDINTERFACE.