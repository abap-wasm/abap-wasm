CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS create_with_wasm
      IMPORTING
        !iv_wasm       TYPE xstring
      RETURNING
        VALUE(ro_wasm) TYPE REF TO zcl_wasm .
    CLASS-METHODS create_with_wast
      IMPORTING
        !iv_wast       TYPE string
      RETURNING
        VALUE(ro_wasm) TYPE REF TO zcl_wasm .
    CLASS-METHODS execute_wast
      IMPORTING
        !iv_wast TYPE string .
    METHODS constructor
      IMPORTING
        !iv_wasm TYPE xstring .
    METHODS execute_export .
    METHODS list_exports .
    METHODS list_imports .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM IMPLEMENTATION.


  METHOD constructor.
* todo
    RETURN.
  ENDMETHOD.


  METHOD create_with_wasm.

    ro_wasm = NEW zcl_wasm( iv_wasm ).

  ENDMETHOD.


  METHOD create_with_wast.

    ro_wasm = create_with_wasm( NEW zcl_wast( iv_wast )->to_wasm( ) ).

  ENDMETHOD.


  METHOD execute_export.
* todo
    RETURN.
  ENDMETHOD.


  METHOD execute_wast.
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
