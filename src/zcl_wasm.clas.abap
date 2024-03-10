CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_wasm_module.

    TYPES: BEGIN OF ty_import,
             name   TYPE string,
             module TYPE REF TO object, " todo
           END OF ty_import.
    TYPES ty_imports_tt TYPE STANDARD TABLE OF ty_import WITH DEFAULT KEY.

    CLASS-METHODS create_with_wasm
      IMPORTING
        !iv_wasm       TYPE xstring
        it_imports     TYPE ty_imports_tt OPTIONAL
      RETURNING
        VALUE(ri_wasm) TYPE REF TO zif_wasm_module
      RAISING
        zcx_wasm.

    CLASS-METHODS create_with_base64
      IMPORTING
        !iv_base64     TYPE string
        it_imports     TYPE ty_imports_tt OPTIONAL
      RETURNING
        VALUE(ri_wasm) TYPE REF TO zif_wasm_module
      RAISING
        zcx_wasm.

    METHODS constructor
      IMPORTING
        !io_module TYPE REF TO zcl_wasm_module .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_module TYPE REF TO zcl_wasm_module.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.
ENDCLASS.



CLASS zcl_wasm IMPLEMENTATION.


  METHOD constructor.
    mo_module = io_module.
  ENDMETHOD.


  METHOD create_with_base64.

    DATA lv_xstr TYPE xstring.

* ABAP Cloud compatible decoding,
    TRY.
        CALL METHOD ('CL_WEB_HTTP_UTILITY')=>('DECODE_X_BASE64')
          EXPORTING
            encoded = iv_base64
          RECEIVING
            decoded = lv_xstr.
      CATCH cx_sy_dyn_call_illegal_class.
        DATA(lv_classname) = 'CL_HTTP_UTILITY'.
        CALL METHOD (lv_classname)=>('DECODE_X_BASE64')
          EXPORTING
            encoded = iv_base64
          RECEIVING
            decoded = lv_xstr.
    ENDTRY.

    ri_wasm = NEW zcl_wasm( NEW zcl_wasm_parser( )->parse( lv_xstr ) ).

  ENDMETHOD.


  METHOD create_with_wasm.
    ri_wasm = NEW zcl_wasm( NEW zcl_wasm_parser( )->parse( iv_wasm ) ).
  ENDMETHOD.


  METHOD zif_wasm_module~execute_function_export.

    DATA lv_got TYPE xstring.

    DATA(ls_export) = mo_module->get_export_by_name( iv_name ).
    IF ls_export-type <> zif_wasm_types=>c_export_type-func.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'execute_function_export: expected type func'.
    ENDIF.

    DATA(ls_function) = mo_module->get_function_by_index( ls_export-index ).
    DATA(ls_type) = mo_module->get_type_by_index( CONV #( ls_function-typeidx ) ).

    IF lines( it_parameters ) <> xstrlen( ls_type-parameter_types ).
      LOOP AT it_parameters INTO DATA(li_param).
        DATA(lv_type) = li_param->get_type( ).
        CONCATENATE lv_got lv_type INTO lv_got IN BYTE MODE.
      ENDLOOP.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |execute_function_export: number of parameters doesnt match, expected {
        ls_type-parameter_types }, got { lv_got }|.
    ENDIF.

    IF mo_memory IS INITIAL.
      zif_wasm_module~instantiate( ).
    ENDIF.

    LOOP AT it_parameters INTO DATA(li_value).
      mo_memory->get_stack( )->push( li_value ).
    ENDLOOP.

    NEW zcl_wasm_vm(
      io_memory = mo_memory
      io_module = mo_module )->call( ls_export-index ).

    DO xstrlen( ls_type-result_types ) TIMES.
      INSERT mo_memory->get_stack( )->pop( ) INTO rt_results INDEX 1.
    ENDDO.

  ENDMETHOD.


  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.


  METHOD zif_wasm_module~instantiate.
* https://webassembly.github.io/spec/core/exec/modules.html#instantiation

    ASSERT mo_memory IS INITIAL.
    mo_memory = NEW zcl_wasm_memory( ).

* The improts component of a module defines a set of imports that are required for instantiation.
    mo_module->get_import_section( )->import( mo_memory ).
* do instantiation
    mo_module->get_memory_section( )->instantiate( mo_memory ).
    mo_module->get_global_section( )->instantiate( mo_memory ).
    mo_module->get_data_section( )->instantiate( mo_memory ).
    mo_module->get_table_section( )->instantiate( mo_memory ).
    mo_module->get_element_section( )->instantiate( mo_memory ).
  ENDMETHOD.

ENDCLASS.
