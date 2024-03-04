CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS mul_0x10000000_4096 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD mul_0x10000000_4096.

    " (module
    " (func (export "mul") (result i32)
    "   i32.const 0x10000000
    "   i32.const 4096
    "   i32.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNtdWwAAAoOAQwAQYCAgIABQYAgbAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = 0 ).
  ENDMETHOD.

ENDCLASS.
