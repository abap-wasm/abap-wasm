CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS convert FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD convert.

" (module
"   (func (export "convert") (result f64)
"     i64.const 1
"     f64.convert_i64_s)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF8AwIBAAcLAQdjb252ZXJ0AAAKBwEFAEIBuQsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'convert' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_f64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_value( )
      exp = 1 ).
  ENDMETHOD.

ENDCLASS.
