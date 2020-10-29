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

    DATA(lo_parser) = NEW zcl_wasm_parser( ).
    DATA(lo_module) = lo_parser->parse( iv_wasm ).
    ro_wasm = NEW zcl_wasm( lo_module ).

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

    DATA li_value TYPE REF TO zif_wasm_value.
    DATA ls_export TYPE zcl_wasm_module=>ty_export.
    DATA ls_code TYPE zcl_wasm_module=>ty_code.

    ls_export = mo_module->get_export_by_name( iv_name ).
    ls_code = mo_module->get_code_by_index( ls_export-index ).

    DATA(lo_memory) = NEW zcl_wasm_memory( ).

    LOOP AT it_parameters INTO li_value.
      lo_memory->local_push( li_value ).
    ENDLOOP.

    DATA(lo_vm) = NEW zcl_wasm_vm( lo_memory ).
    lo_vm->execute( ls_code-instructions ).

*    APPEND lo_memory->pop( ) TO rt_results.

  ENDMETHOD.


  METHOD list_function_exports.

    DATA ls_function TYPE ty_name_and_parameter.
    DATA ls_export TYPE zcl_wasm_module=>ty_export.

    DATA(lt_exports) = mo_module->get_exports( ).
    LOOP AT lt_exports INTO ls_export.
      IF ls_export-type = zcl_wasm_types=>c_export_type-func.
        CLEAR ls_function.
        ls_function-name = ls_export-name.
* todo
*        mo_module->get_function_by_index( ls_export-index ).
        APPEND ls_function TO rt_functions.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
