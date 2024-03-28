CLASS cl_jsonschema DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_hex         TYPE xstring
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS cl_jsonschema IMPLEMENTATION.

  METHOD run.

    GET RUN TIME FIELD DATA(lv_start).
    DATA(li_wasm) = zcl_wasm=>create_with_wasm(
      iv_wasm    = iv_hex
      it_imports = VALUE #( (
        name   = '__wbindgen_placeholder__'
        module = NEW cl_wbindgen_placeholder( ) ) ) ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_parsing) = lv_end - lv_start.
    WRITE / |{ lv_parsing }ms parsing jsonschema|.

    rv_json = '{"parsing": "' && lv_parsing && '"}'.

    " WRITE / 'call __wbindgen_add_to_stack_pointer'.
    " DATA(lt_results) = li_wasm->execute_function_export(
    "   iv_name       = '__wbindgen_add_to_stack_pointer'
    "   it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( -16 ) ) ) ).
    " DATA(lo_retptr) = CAST zcl_wasm_i32( lt_results[ 1 ] ).
    " WRITE / |retptr: { lo_retptr->get_signed( ) }|.

    " WRITE / 'call main'.
    " li_wasm->execute_function_export(
    "   iv_name       = 'main'
    "   it_parameters = VALUE #( ( lo_retptr ) ) ).

    " DATA(li_linear) = li_wasm->get_memory( )->mi_linear.
    " DATA(lv_realptr) = li_linear->get(
    "   iv_length = 4
    "   iv_offset = lo_retptr->get_signed( ) ).
    " " WRITE / lv_realptr.

    " DATA(lv_reallen) = li_linear->get(
    "   iv_length = 4
    "   iv_offset = lo_retptr->get_signed( ) + 4 ).
    " " WRITE / lv_reallen.

    " DATA(lv_return) = li_linear->get(
    "   iv_length = CONV #( lv_reallen )
    "   iv_offset = CONV #( lv_realptr ) ).
    " DATA(lv_str) = cl_abap_codepage=>convert_from( zcl_wasm_binary_stream=>reverse_hex( lv_return ) ).
    " WRITE / lv_str.

  ENDMETHOD.

ENDCLASS.
