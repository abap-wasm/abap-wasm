CLASS zcl_wasm_wast DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS to_wasm
      RETURNING
        VALUE(rv_wasm) TYPE xstring .
    METHODS constructor
      IMPORTING
        !iv_wast TYPE string .
    METHODS get_first_module
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module .
    METHODS list_modules .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_WAST IMPLEMENTATION.


  METHOD constructor.
* todo
    RETURN.
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
