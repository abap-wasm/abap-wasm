CLASS zcl_wasm_text_to_binary DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_wast TYPE string .
    METHODS run
      RETURNING
        VALUE(rv_wasm) TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_TEXT_TO_BINARY IMPLEMENTATION.


  METHOD constructor.
* todo
  ENDMETHOD.


  METHOD run.
* todo
  ENDMETHOD.
ENDCLASS.
