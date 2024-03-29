CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_00030003 FOR TESTING RAISING cx_static_check.
    METHODS test_large FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_00030003.

" (module
"   (func (export "mul") (result i64)
"     i64.const 0x0000000300000003
"     i64.const 0x0000000300000003
"     i64.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNtdWwAAAoRAQ8AQoOAgIAwQoOAgIAwfgsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 77309411337 ).
  ENDMETHOD.

  METHOD test_large.

" (module
"   (func (export "mul") (result i64)
"     i64.const 0x00FF000300000003
"     i64.const 0x00FF000300000003
"     i64.mul))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNtdWwAAAoZARcAQoOAgICwgMD/AEKDgICAsIDA/wB+CwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'mul' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 430656791676715017 ).
  ENDMETHOD.

ENDCLASS.
