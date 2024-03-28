CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS add_2147483647_2147483647 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD add_2147483647_2147483647.

    " (module
    " (func (export "add") (result i32)
    "   i32.const 2147483647
    "   i32.const 2147483647
    "   i32.add))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNhZGQAAAoRAQ8AQf////8HQf////8HagsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'add' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = -2 ).

  ENDMETHOD.

ENDCLASS.
