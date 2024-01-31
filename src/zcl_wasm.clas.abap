CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS create_with_wasm
      IMPORTING
        !iv_wasm       TYPE xstring
      RETURNING
        VALUE(ri_wasm) TYPE REF TO zif_wasm
      RAISING
        zcx_wasm.

    CLASS-METHODS create_with_base64
      IMPORTING
        !iv_base64     TYPE string
      RETURNING
        VALUE(ri_wasm) TYPE REF TO zif_wasm
      RAISING
        zcx_wasm.

    METHODS constructor
      IMPORTING
        !io_module TYPE REF TO zcl_wasm_module .

    INTERFACES zif_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_module TYPE REF TO zcl_wasm_module .
ENDCLASS.



CLASS zcl_wasm IMPLEMENTATION.


  METHOD constructor.

    mo_module = io_module.

  ENDMETHOD.


  METHOD create_with_wasm.

    ri_wasm = NEW zcl_wasm( NEW zcl_wasm_parser( )->parse( iv_wasm ) ).

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


  METHOD zif_wasm~execute_function_export.

    DATA(ls_export) = mo_module->get_export_by_name( iv_name ).
    IF ls_export-type <> zcl_wasm_types=>c_export_type-func.
      RAISE EXCEPTION NEW zcx_wasm( text = 'execute_function_export: expected type func' ).
    ENDIF.

    DATA(lv_type) = mo_module->get_function_by_index( ls_export-index ).
    DATA(ls_type) = mo_module->get_type_by_index( CONV #( lv_type ) ).

    DATA(lo_memory) = NEW zcl_wasm_memory( ).

    IF lines( it_parameters ) <> xstrlen( ls_type-parameter_types ).
      RAISE EXCEPTION NEW zcx_wasm( text = 'execute_function_export: number of parameters doesnt match' ).
    ENDIF.

    LOOP AT it_parameters INTO DATA(li_value).
      lo_memory->stack_push( li_value ).
    ENDLOOP.

    NEW zcl_wasm_vm(
      io_memory = lo_memory
      io_module = mo_module )->call( ls_export-index ).

    DO xstrlen( ls_type-result_types ) TIMES.
      APPEND lo_memory->stack_pop( ) TO rt_results.
    ENDDO.

  ENDMETHOD.


  METHOD zif_wasm~list_function_exports.

    DATA ls_function TYPE zif_wasm=>ty_name_and_parameter.

    LOOP AT mo_module->get_exports( ) INTO DATA(ls_export).
      IF ls_export-type = zcl_wasm_types=>c_export_type-func.
        CLEAR ls_function.
        ls_function-name = ls_export-name.
        DATA(lv_type_index) = mo_module->get_function_by_index( ls_export-index ).
*        ls_function-parameters =
        APPEND ls_function TO rt_functions.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
