INTERFACE zif_wasm_instruction PUBLIC.

  TYPES ty_list TYPE STANDARD TABLE OF REF TO zif_wasm_instruction WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_control,
           control TYPE string,
           depth   TYPE i,
         END OF ty_control.

  CONSTANTS: BEGIN OF c_control,
               return_ TYPE string VALUE 'RETURN',
               branch  TYPE string VALUE 'BRANCH',
             END OF c_control.

  METHODS execute
    IMPORTING
      io_memory  TYPE REF TO zcl_wasm_memory
      io_module  TYPE REF TO zcl_wasm_module
    CHANGING
      cs_control TYPE ty_control
    RAISING
      zcx_wasm.

ENDINTERFACE.
