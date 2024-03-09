CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

    " (module
    " (func (export "name") (result i64)
    "   i64.const 0x80
    "   i64.extend8_s))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcIAQRuYW1lAAAKCAEGAEKAAcILAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = -128 ).
  ENDMETHOD.

ENDCLASS.
