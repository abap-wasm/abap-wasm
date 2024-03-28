CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS basic FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD basic.

" (module
"   (func (export "ret") (param i32) (result i32)
"     block  ;; label = @1
"       i32.const 99
"       return
"     end
"     unreachable
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAX8BfwMCAQAHBwEDcmV0AAAKDAEKAAJAQeMADwsACwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'ret'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( 2 ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 99 ).

  ENDMETHOD.

ENDCLASS.
