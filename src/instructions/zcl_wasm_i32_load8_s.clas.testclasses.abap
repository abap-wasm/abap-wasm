CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS negative FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD negative.

" (module
"   (func (export "name") (result i32)
"     i32.const 0
"     i32.const -1
"     i32.store
"     i32.const 0
"     i32.load8_s)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBwgBBG5hbWUAAAoQAQ4AQQBBfzYCAEEALAAACwAKBG5hbWUCAwEAAA==`.

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
