CLASS zcl_wasm DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS create_with_wasm
      IMPORTING
        !iv_wasm       TYPE xstring
        it_imports     TYPE zif_wasm_types=>ty_imports_tt OPTIONAL
      RETURNING
        VALUE(ri_wasm) TYPE REF TO zif_wasm_module
      RAISING
        zcx_wasm.

    CLASS-METHODS create_with_base64
      IMPORTING
        !iv_base64     TYPE string
        it_imports     TYPE zif_wasm_types=>ty_imports_tt OPTIONAL
      RETURNING
        VALUE(ri_wasm) TYPE REF TO zif_wasm_module
      RAISING
        zcx_wasm.

ENDCLASS.



CLASS zcl_wasm IMPLEMENTATION.

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

    ri_wasm = create_with_wasm(
      iv_wasm    = lv_xstr
      it_imports = it_imports ).
  ENDMETHOD.

  METHOD create_with_wasm.
    IF iv_wasm IS INITIAL.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'create_with_wasm: empty input'.
    ENDIF.

    DATA(lo_module) = NEW zcl_wasm_parser( )->parse( iv_wasm ).

    lo_module->register_imports( it_imports ).

    ri_wasm = lo_module.
  ENDMETHOD.

ENDCLASS.
