CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    METHODS execute_export .
    METHODS list_exports .
    METHODS list_imports .
    CLASS-METHODS create_with_binary
      IMPORTING
        !iv_wasm TYPE xstring .
    CLASS-METHODS create_with_text
      IMPORTING
        !iv_wast TYPE string .
    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD create_with_binary.

* todo

  ENDMETHOD.


  METHOD create_with_text.

* todo, call new class to convert text to binary

  ENDMETHOD.


  METHOD execute_export.
  ENDMETHOD.


  METHOD list_exports.

* todo

  ENDMETHOD.


  METHOD list_imports.
  ENDMETHOD.
ENDCLASS.
