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



CLASS zcl_wasm_text_to_binary IMPLEMENTATION.


  METHOD constructor.
* todo
    RETURN.
  ENDMETHOD.


  METHOD run.
* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
