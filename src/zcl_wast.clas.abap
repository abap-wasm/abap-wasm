CLASS zcl_wast DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_wast TYPE string .
    METHODS get_first_module
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module .
    METHODS list_modules .
    METHODS to_wasm
      RETURNING
        VALUE(rv_wasm) TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WAST IMPLEMENTATION.


  METHOD constructor.

    NEW zcl_wast_parser( )->parse( iv_wast ).

  ENDMETHOD.


  METHOD get_first_module.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_modules.
  ENDMETHOD.


  METHOD to_wasm.

* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
