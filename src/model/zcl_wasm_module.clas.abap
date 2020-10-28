CLASS zcl_wasm_module DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_type,
        parameter_types TYPE xstring,
        result_types    TYPE xstring,
      END OF ty_type .
    TYPES:
      ty_types TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_code,
        locals       TYPE xstring,
        instructions TYPE xstring,
      END OF ty_code .
    TYPES:
      ty_codes TYPE STANDARD TABLE OF ty_code WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_export,
        name  TYPE string,
        type  TYPE x LENGTH 1,
        index TYPE i,
      END OF ty_export .
    TYPES:
      ty_exports TYPE STANDARD TABLE OF ty_export WITH DEFAULT KEY .
    TYPES:
      ty_functions TYPE STANDARD TABLE OF i WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !it_types     TYPE ty_types OPTIONAL
        !it_codes     TYPE ty_codes OPTIONAL
        !it_exports   TYPE ty_exports OPTIONAL
        !it_functions TYPE ty_functions OPTIONAL .
    METHODS get_types
      RETURNING
        VALUE(rt_result) TYPE ty_types .
    METHODS get_codes
      RETURNING
        VALUE(rt_result) TYPE ty_codes .
    METHODS get_exports
      RETURNING
        VALUE(rt_result) TYPE ty_exports .
    METHODS get_functions
      RETURNING
        VALUE(rt_result) TYPE ty_functions .
    METHODS get_code_by_index
      IMPORTING
        !iv_index      TYPE i
      RETURNING
        VALUE(rs_code) TYPE ty_code.
    METHODS get_function_by_index
      IMPORTING
        !iv_index      TYPE i
      RETURNING
        VALUE(rv_type) TYPE i .
    METHODS get_export_by_name
      IMPORTING
        !iv_name         TYPE string
      RETURNING
        VALUE(rs_export) TYPE ty_export .
    METHODS get_type_by_index
      IMPORTING
        !iv_index      TYPE i
      RETURNING
        VALUE(rs_type) TYPE ty_type .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_types TYPE ty_types .
    DATA mt_codes TYPE ty_codes .
    DATA mt_exports TYPE ty_exports .
    DATA mt_functions TYPE ty_functions .
ENDCLASS.



CLASS ZCL_WASM_MODULE IMPLEMENTATION.


  METHOD constructor.
    mt_types    = it_types.
    mt_codes     = it_codes.
    mt_exports   = it_exports.
    mt_functions = it_functions.
  ENDMETHOD.


  METHOD get_codes.
    rt_result = mt_codes.
  ENDMETHOD.


  METHOD get_code_by_index.

* index is zero based
    DATA(lv_index) = iv_index + 1.

    READ TABLE mt_codes INDEX lv_index INTO rs_code.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_exports.
    rt_result = mt_exports.
  ENDMETHOD.


  METHOD get_export_by_name.

* todo
*    READ TABLE mt_exports WITH KEY name = iv_name INTO rs_export.
    READ TABLE mt_exports INDEX 1 INTO rs_export.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_functions.
    rt_result = mt_functions.
  ENDMETHOD.


  METHOD get_function_by_index.

* index is zero based
    DATA(lv_index) = iv_index + 1.

    READ TABLE mt_functions INDEX lv_index INTO rv_type.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_types.
    rt_result = mt_types.
  ENDMETHOD.


  METHOD get_type_by_index.

* index is zero based
    DATA(lv_index) = iv_index + 1.

    READ TABLE mt_types INDEX lv_index INTO rs_type.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
