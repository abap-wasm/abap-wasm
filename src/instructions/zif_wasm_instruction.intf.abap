INTERFACE zif_wasm_instruction PUBLIC.

  TYPES ty_list TYPE STANDARD TABLE OF REF TO zif_wasm_instruction WITH DEFAULT KEY.

  TYPES ty_control TYPE string.

  CONSTANTS: BEGIN OF c_control,
               return_ TYPE ty_control VALUE 'RETURN',
             END OF c_control.

  METHODS execute
    IMPORTING
      io_memory         TYPE REF TO zcl_wasm_memory
      io_module         TYPE REF TO zcl_wasm_module
    RETURNING
      VALUE(rv_control) TYPE ty_control
    RAISING
      zcx_wasm
      zcx_wasm_branch.

ENDINTERFACE.
