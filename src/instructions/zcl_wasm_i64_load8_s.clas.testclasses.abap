CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS negative FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD negative.

  "   (module
  "   (func (export "load8_s") (param i64) (result i64)
  "     i32.const 8
  "     local.get 0
  "     i64.store8
  "     i32.const 8
  "     i64.load8_s)
  " (memory (;0;) 1))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAX4BfgMCAQAFAwEAAQcLAQdsb2FkOF9zAAAKEAEOAEEIIAA8AABBCDAAAAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'load8_s'
      it_parameters = VALUE #( ( zcl_wasm_i64=>from_signed( -1 ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = -1 ).
  ENDMETHOD.

ENDCLASS.
