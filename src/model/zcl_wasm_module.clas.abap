CLASS zcl_wasm_module DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_type,
             parameter_types TYPE xstring,
             result_types    TYPE xstring,
           END OF ty_type.
    TYPES: ty_types TYPE STANDARD TABLE OF ty_type WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_code,
             locals       TYPE xstring,
             instructions TYPE xstring,
           END OF ty_code.
    TYPES: ty_codes TYPE STANDARD TABLE OF ty_code WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_export,
             name  TYPE string,
             type  TYPE x LENGTH 1,
             index TYPE i,
           END OF ty_export.
    TYPES: ty_exports TYPE STANDARD TABLE OF ty_export WITH DEFAULT KEY.

    TYPES: ty_functions TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS constructor.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_MODULE IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
