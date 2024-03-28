CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS sub_n2147483648_n2147483648 FOR TESTING RAISING cx_static_check.
    METHODS sub_n2147483648_2147483648 FOR TESTING RAISING cx_static_check.
    METHODS sub_another FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD sub_n2147483648_n2147483648.

    " (module
    " (func (export "sub") (result i32)
    "   i32.const -2147483648
    "   i32.const -2147483648
    "   i32.sub))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNzdWIAAAoRAQ8AQYCAgIB4QYCAgIB4awsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 0 ).

  ENDMETHOD.

  METHOD sub_n2147483648_2147483648.

    " (module
    " (func (export "sub") (result i32)
    "   i32.const -2147483648
    "   i32.const 2147483648
    "   i32.sub))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNzdWIAAAoRAQ8AQYCAgIB4QYCAgIB4awsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 0 ).

  ENDMETHOD.

  METHOD sub_another.

    " (module
    " (func (export "sub") (result i32)
    "   i32.const -2147483648
    "   i32.const 21474836
    "   i32.sub))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNzdWIAAAoQAQ4AQYCAgIB4QZTcngprCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 2126008812 ).

  ENDMETHOD.

ENDCLASS.
