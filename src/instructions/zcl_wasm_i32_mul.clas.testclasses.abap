CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS mul_0x10000000_4096 FOR TESTING RAISING cx_static_check.
    METHODS mul_2222222222_2222222222 FOR TESTING RAISING cx_static_check.
    METHODS mul_2222222222_n22 FOR TESTING RAISING cx_static_check.
    METHODS mul_2147483647_2147483647 FOR TESTING RAISING cx_static_check.
    METHODS mul_2147483647_2147483646 FOR TESTING RAISING cx_static_check.

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
      act = lo_value->mv_value
      exp = 0 ).
  ENDMETHOD.

  METHOD mul_2222222222_2222222222.

    " (module
    " (func (export "mul") (result i32)
    "   i32.const 2222222222
    "   i32.const 2222222222
    "   i32.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNtdWwAAAoRAQ8AQY7X0aN4QY7X0aN4bAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 367002308 ).

  ENDMETHOD.

  METHOD mul_2222222222_n22.

    " (module
    " (func (export "mul") (result i32)
    "   i32.const 2222222222
    "   i32.const -22
    "   i32.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNtdWwAAAoNAQsAQY7X0aN4QWpsCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -1644248628 ).

  ENDMETHOD.

  METHOD mul_2147483647_2147483647.

    " (module
    " (func (export "mul") (result i32)
    "   i32.const 2147483647
    "   i32.const 2147483647
    "   i32.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNtdWwAAAoRAQ8AQf////8HQf////8HbAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 1 ).

  ENDMETHOD.

  METHOD mul_2147483647_2147483646.

    " (module
    " (func (export "mul") (result i32)
    "   i32.const 2147483647
    "   i32.const 2147483646
    "   i32.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNtdWwAAAoRAQ8AQf////8HQf7///8HbAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -2147483646 ).

  ENDMETHOD.

ENDCLASS.
