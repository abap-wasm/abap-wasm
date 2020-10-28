CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_name_and_parameter,
             name       TYPE string,
             parameters TYPE xstring,
           END OF ty_name_and_parameter.
    TYPES ty_name_and_parameters TYPE STANDARD TABLE OF ty_name_and_parameter WITH DEFAULT KEY.

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
        !io_module TYPE REF TO zcl_wasm_module .
    METHODS execute_function_export
      IMPORTING
        !iv_name          TYPE string
        !it_parameters    TYPE zif_wasm_value=>ty_values
      RETURNING
        VALUE(rt_results) TYPE zif_wasm_value=>ty_values .
    METHODS list_function_exports
      RETURNING
        VALUE(rt_functions) TYPE ty_name_and_parameters.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_module TYPE REF TO zcl_wasm_module .
ENDCLASS.



CLASS ZCL_WASM IMPLEMENTATION.


  METHOD constructor.

    mo_module = io_module.

  ENDMETHOD.


  METHOD create_with_wasm.

    ro_wasm = NEW #( NEW zcl_wasm_parser( )->parse( iv_wasm ) ).

  ENDMETHOD.


  METHOD create_with_wast.

* todo
    ASSERT 0 = 1.

  ENDMETHOD.


  METHOD create_with_wat.

* todo
    ASSERT 0 = 1.

  ENDMETHOD.


  METHOD execute_function_export.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_function_exports.

    DATA: ls_function TYPE ty_name_and_parameter.

    LOOP AT mo_module->get_exports( ) INTO DATA(ls_export).
      IF ls_export-type = zcl_wasm_types=>c_export_type-func.
        CLEAR rt_functions.
        ls_function-name = ls_export-name.
*        BREAK-POINT.
*        mo_module->get_function_by_index( ls_export-index ).
        APPEND ls_function TO rt_functions.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
