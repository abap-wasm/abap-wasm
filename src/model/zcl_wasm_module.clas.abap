CLASS zcl_wasm_module DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_module.

    TYPES:
      BEGIN OF ty_type,
        parameter_types TYPE xstring,
        result_types    TYPE xstring,
      END OF ty_type .
    TYPES:
      ty_types TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY .
    TYPES: BEGIN OF ty_local,
              count TYPE i,
              type  TYPE zif_wasm_types=>ty_valtype ,
           END OF ty_local.
    TYPES: ty_locals TYPE STANDARD TABLE OF ty_local WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_code,
        locals       TYPE ty_locals,
        instructions TYPE zif_wasm_instruction=>ty_list,
      END OF ty_code .
    TYPES:
      ty_codes TYPE STANDARD TABLE OF ty_code WITH DEFAULT KEY .

    TYPES: BEGIN OF ty_function,
             typeidx       TYPE i,
             codeidx       TYPE i,
             extern_module TYPE string,
             extern_name   TYPE string,
           END OF ty_function.
    TYPES ty_functions TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !it_types          TYPE ty_types OPTIONAL
        !it_codes          TYPE ty_codes OPTIONAL
        !it_exports        TYPE zif_wasm_module=>ty_exports OPTIONAL
        io_data_section    TYPE REF TO zcl_wasm_data_section OPTIONAL
        io_memory_section  TYPE REF TO zcl_wasm_memory_section OPTIONAL
        io_global_section  TYPE REF TO zcl_wasm_global_section OPTIONAL
        io_import_section  TYPE REF TO zcl_wasm_import_section OPTIONAL
        io_table_section   TYPE REF TO zcl_wasm_table_section OPTIONAL
        io_element_section TYPE REF TO zcl_wasm_element_section OPTIONAL
        !it_functions      TYPE ty_functions OPTIONAL .

    METHODS get_types
      RETURNING
        VALUE(rt_result) TYPE ty_types .
    METHODS get_codes
      RETURNING
        VALUE(rt_result) TYPE ty_codes .
    METHODS get_exports
      RETURNING
        VALUE(rt_result) TYPE zif_wasm_module=>ty_exports .
    METHODS get_functions
      RETURNING
        VALUE(rt_result) TYPE ty_functions .
    METHODS get_data_section
      RETURNING
        VALUE(ro_data_section) TYPE REF TO zcl_wasm_data_section.
    METHODS get_memory_section
      RETURNING
        VALUE(ro_memory_section) TYPE REF TO zcl_wasm_memory_section.
    METHODS get_global_section
      RETURNING
        VALUE(ro_global_section) TYPE REF TO zcl_wasm_global_section.
    METHODS get_import_section
      RETURNING
        VALUE(ro_import_section) TYPE REF TO zcl_wasm_import_section.
    METHODS get_table_section
      RETURNING
        VALUE(ro_table_section) TYPE REF TO zcl_wasm_table_section.
    METHODS get_element_section
      RETURNING
        VALUE(ro_element_section) TYPE REF TO zcl_wasm_element_section.
    METHODS get_code_by_index
      IMPORTING
        !iv_index      TYPE int8
      RETURNING
        VALUE(rr_code) TYPE REF TO ty_code
      RAISING
        zcx_wasm.
    METHODS get_function_by_index
      IMPORTING
        iv_index           TYPE int8
      RETURNING
        VALUE(rs_function) TYPE ty_function
      RAISING
        zcx_wasm.
    METHODS get_type_by_index
      IMPORTING
        !iv_index      TYPE int8
      RETURNING
        VALUE(rs_type) TYPE ty_type
      RAISING
        zcx_wasm.

    METHODS execute_instructions
      IMPORTING
        !it_instructions TYPE zif_wasm_instruction=>ty_list
      CHANGING
        cs_control       TYPE zif_wasm_instruction=>ty_control
      RAISING
        zcx_wasm.

    METHODS register_imports
      IMPORTING
        !it_imports TYPE zif_wasm_types=>ty_imports_tt .
* throws if not found
    METHODS get_import_by_module_name
      IMPORTING
        !iv_module_name  TYPE string
      RETURNING
        VALUE(ri_module) TYPE REF TO zif_wasm_module
      RAISING
        zcx_wasm.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.

    DATA mt_types TYPE ty_types .
    DATA mt_codes TYPE ty_codes .
    DATA mt_exports TYPE zif_wasm_module=>ty_exports .
    DATA mt_functions TYPE ty_functions .
    DATA mt_imports TYPE zif_wasm_types=>ty_imports_tt.

    DATA mo_data_section TYPE REF TO zcl_wasm_data_section.
    DATA mo_memory_section TYPE REF TO zcl_wasm_memory_section.
    DATA mo_global_section TYPE REF TO zcl_wasm_global_section.
    DATA mo_import_section TYPE REF TO zcl_wasm_import_section.
    DATA mo_table_section TYPE REF TO zcl_wasm_table_section.
    DATA mo_element_section TYPE REF TO zcl_wasm_element_section.
ENDCLASS.



CLASS zcl_wasm_module IMPLEMENTATION.

  METHOD constructor.
    mt_types        = it_types.
    mt_codes        = it_codes.
    mt_exports      = it_exports.
    mt_functions    = it_functions.

    IF io_data_section IS INITIAL.
* none specified, create the empty data section,
      mo_data_section = NEW #( ).
    ELSE.
      mo_data_section = io_data_section.
    ENDIF.

    IF io_memory_section IS INITIAL.
* none specified, create the empty data section,
      mo_memory_section = NEW #( ).
    ELSE.
      mo_memory_section = io_memory_section.
    ENDIF.

    IF io_global_section IS INITIAL.
* none specified, create the empty data section,
      mo_global_section = NEW #( ).
    ELSE.
      mo_global_section = io_global_section.
    ENDIF.

    IF io_import_section IS INITIAL.
* none specified, create the empty data section,
      mo_import_section = NEW #( ).
    ELSE.
      mo_import_section = io_import_section.
    ENDIF.

    IF io_table_section IS INITIAL.
* none specified, create the empty data section,
      mo_table_section = NEW #( ).
    ELSE.
      mo_table_section = io_table_section.
    ENDIF.

    IF io_element_section IS INITIAL.
* none specified, create the empty data section,
      mo_element_section = NEW #( ).
    ELSE.
      mo_element_section = io_element_section.
    ENDIF.

  ENDMETHOD.

  METHOD register_imports.
    mt_imports = it_imports.
  ENDMETHOD.

  METHOD get_import_by_module_name.
    READ TABLE mt_imports WITH TABLE KEY name = iv_module_name INTO DATA(ls_import).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |get_import_by_module_name: not found, { iv_module_name }|.
    ENDIF.
    ri_module = ls_import-module.
  ENDMETHOD.

  METHOD get_element_section.
    ro_element_section = mo_element_section.
  ENDMETHOD.

  METHOD get_table_section.
    ro_table_section = mo_table_section.
  ENDMETHOD.

  METHOD get_import_section.
    ro_import_section = mo_import_section.
  ENDMETHOD.

  METHOD get_data_section.
    ro_data_section = mo_data_section.
  ENDMETHOD.

  METHOD get_memory_section.
    ro_memory_section = mo_memory_section.
  ENDMETHOD.

  METHOD get_codes.
    rt_result = mt_codes.
  ENDMETHOD.

  METHOD get_code_by_index.

    IF iv_index < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |get_code_by_index: negative index, { iv_index }|.
    ENDIF.

* index is zero based
    DATA(lv_index) = iv_index + 1.

    READ TABLE mt_codes INDEX lv_index REFERENCE INTO rr_code.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |get_code_by_index: not found, { lv_index }|.
    ENDIF.

  ENDMETHOD.

  METHOD get_global_section.
    ro_global_section = mo_global_section.
  ENDMETHOD.

  METHOD get_exports.
    rt_result = mt_exports.
  ENDMETHOD.


  METHOD zif_wasm_module~get_export_by_name.

    READ TABLE mt_exports WITH TABLE KEY name = iv_name INTO rs_export.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |get_export_by_name, not found: { iv_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_functions.
    rt_result = mt_functions.
  ENDMETHOD.


  METHOD get_function_by_index.

* index is zero based
    DATA(lv_index) = iv_index + 1.

    READ TABLE mt_functions INDEX lv_index INTO rs_function.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |get_function_by_index: not found, { lv_index }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_types.
    rt_result = mt_types.
  ENDMETHOD.


  METHOD get_type_by_index.

* index is zero based
    DATA(lv_index) = iv_index + 1.

    READ TABLE mt_types INDEX lv_index INTO rs_type.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'get_type_by_index: not found'.
    ENDIF.

  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.


  METHOD zif_wasm_module~instantiate.
* https://webassembly.github.io/spec/core/exec/modules.html#instantiation

    ASSERT mo_memory IS INITIAL.
    mo_memory = NEW zcl_wasm_memory( ).

* The imports component of a module defines a set of imports that are required for instantiation.
    get_import_section( )->import(
      io_memory  = mo_memory
      it_imports = mt_imports ).

* do instantiation
    get_memory_section( )->instantiate( mo_memory ).
    get_global_section( )->instantiate( mo_memory ).
    get_data_section( )->instantiate( mo_memory ).
    get_table_section( )->instantiate( mo_memory ).
    get_element_section( )->instantiate( mo_memory ).

    ri_module = me.
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.

    DATA lv_got TYPE xstring.

    DATA(ls_export) = zif_wasm_module~get_export_by_name( iv_name ).
    IF ls_export-type <> zif_wasm_types=>c_export_type-func.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'execute_function_export: expected type func'.
    ENDIF.

    DATA(ls_function) = get_function_by_index( ls_export-index ).
    DATA(ls_type) = get_type_by_index( CONV #( ls_function-typeidx ) ).

    IF lines( it_parameters ) <> xstrlen( ls_type-parameter_types ).
      LOOP AT it_parameters INTO DATA(li_param).
        DATA(lv_type) = li_param->get_type( ).
        CONCATENATE lv_got lv_type INTO lv_got IN BYTE MODE.
      ENDLOOP.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |execute_function_export: number of parameters doesnt match, expected {
            ls_type-parameter_types }, got { lv_got }|.
    ENDIF.

    IF mo_memory IS INITIAL.
      zif_wasm_module~instantiate( ).
    ENDIF.

    LOOP AT it_parameters INTO DATA(li_value).
      mo_memory->mi_stack->push( li_value ).
    ENDLOOP.

    zcl_wasm_call=>invoke(
      iv_funcidx = ls_export-index
      io_memory  = mo_memory
      io_module  = me ).

    DO xstrlen( ls_type-result_types ) TIMES.
      INSERT mo_memory->mi_stack->pop( ) INTO rt_results INDEX 1.
    ENDDO.

  ENDMETHOD.

  METHOD execute_instructions.
    LOOP AT it_instructions ASSIGNING FIELD-SYMBOL(<li_instruction>).
      " WRITE / '@KERNEL console.dir(fs_li_instruction_.get().constructor.name);'.
      <li_instruction>->execute(
        EXPORTING
          io_memory  = mo_memory
          io_module  = me
        CHANGING
          cs_control = cs_control ).

      IF cs_control-control IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
