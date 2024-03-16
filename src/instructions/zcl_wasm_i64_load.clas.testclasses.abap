CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_1 FOR TESTING RAISING cx_static_check.
    METHODS test_4294967295 FOR TESTING RAISING cx_static_check.
    METHODS test_9219994337134247936 FOR TESTING RAISING cx_static_check.
    METHODS test_minus_1 FOR TESTING RAISING cx_static_check.
    METHODS test_max_negative FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_1.

" (module
"   (func (export "name") (result i64)
"     i32.const 0
"     i32.const 1
"     i32.store
"     i32.const 0
"     i64.load)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAUDAQABBwgBBG5hbWUAAAoQAQ4AQQBBATYCAEEAKQMACwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = 1 ).
  ENDMETHOD.

  METHOD test_4294967295.

" (module
"   (func (export "name") (result i64)
"     i32.const 0
"     i32.const -1
"     i32.store
"     i32.const 0
"     i64.load)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAUDAQABBwgBBG5hbWUAAAoQAQ4AQQBBfzYCAEEAKQMACwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = 4294967295 ).
  ENDMETHOD.

  METHOD test_minus_1.

" (module
"   (func (export "name") (result i64)
"     i32.const 0
"     i32.const -1
"     i32.store
"     i32.const 4
"     i32.const -1
"     i32.store
"     i32.const 0
"     i64.load)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAUDAQABBwgBBG5hbWUAAAoXARUAQQBBfzYCAEEEQX82AgBBACkDAAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = -1 ).

  ENDMETHOD.

  METHOD test_9219994337134247936.

" (module
"   (func (export "name") (result i64)
"     i32.const 0
"     i64.load)
"   (memory (;0;) 1)
"   (data (;0;) (i32.const 0) "\00\00\00\00\00\00\f4\7f")
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAUDAQABBwgBBG5hbWUAAAoJAQcAQQApAwALCw4BAEEACwgAAAAAAAD0fwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = 9219994337134247936 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_unsigned( )
      exp = |9219994337134247936| ).

  ENDMETHOD.

  METHOD test_max_negative.

" (module
"   (func (export "name") (result i64)
"     i32.const 0
"     i64.load)
"   (memory (;0;) 1)
"   (data (;0;) (i32.const 0) "\00\00\00\00\00\00\00\80")
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAUDAQABBwgBBG5hbWUAAAoJAQcAQQApAwALCw4BAEEACwgAAAAAAAAAgAAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = -9223372036854775808 ).

  ENDMETHOD.

ENDCLASS.
