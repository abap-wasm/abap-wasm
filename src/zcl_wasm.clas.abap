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
* todo
    RETURN.
  ENDMETHOD.


  METHOD create_with_binary.
* todo
    RETURN.
  ENDMETHOD.


  METHOD create_with_text.
* todo, call new class to convert text to binary
    RETURN.
  ENDMETHOD.


  METHOD execute_export.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_exports.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_imports.
* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
