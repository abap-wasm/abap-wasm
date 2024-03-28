CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS negative FOR TESTING RAISING cx_static_check.
    METHODS minus_one FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD negative.

    " (module
    " (func (export "name") (result i32)
    "   i32.const 0x8000
    "   i32.extend16_s))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcIAQRuYW1lAAAKCQEHAEGAgALBCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -32768 ).
  ENDMETHOD.

  METHOD minus_one.

    " (module
    " (func (export "name") (result i32)
    "   i32.const 0xFFFF
    "   i32.extend16_s))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcIAQRuYW1lAAAKCQEHAEH//wPBCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -1 ).
  ENDMETHOD.

ENDCLASS.
