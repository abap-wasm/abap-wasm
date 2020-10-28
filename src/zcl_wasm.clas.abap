CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.

    CLASS-METHODS create_with_wasm
      IMPORTING
        !iv_wasm       TYPE xstring
      RETURNING
        VALUE(ro_wasm) TYPE REF TO zcl_wasm .
    CLASS-METHODS create_with_wat
      IMPORTING
        !iv_wast       TYPE string
      RETURNING
        VALUE(ro_wasm) TYPE REF TO zcl_wasm .
    CLASS-METHODS create_with_wast
      IMPORTING
        !iv_wast       TYPE string
      RETURNING
        VALUE(ro_wasm) TYPE REF TO zcl_wasm .
    METHODS constructor
      IMPORTING
        !iv_wasm TYPE xstring .
    METHODS execute_function_export
      IMPORTING
        iv_name           TYPE string
        it_parameters     TYPE zif_wasm_value=>ty_values
      RETURNING
        VALUE(rt_results) TYPE zif_wasm_value=>ty_values.
    METHODS list_function_exports
      RETURNING
        VALUE(rt_functions) TYPE i.
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

* todo
    RETURN.

  ENDMETHOD.


  METHOD create_with_wat.

* todo
    RETURN.

  ENDMETHOD.


  METHOD execute_function_export.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_function_exports.
* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
