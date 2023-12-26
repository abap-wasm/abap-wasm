INTERFACE zif_wasm_instruction PUBLIC.

  TYPES ty_list TYPE STANDARD TABLE OF REF TO zif_wasm_instruction WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_control,
           return_ TYPE abap_bool,
         END OF ty_control.

  METHODS execute
    IMPORTING
      io_memory         TYPE REF TO zcl_wasm_memory
      io_module         TYPE REF TO zcl_wasm_module
    RETURNING
      VALUE(rs_control) TYPE ty_control.

* to_string( ) TYPE string
* to_xstring( ) TYPE xstring

ENDINTERFACE.