CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS basic FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD basic.

" (module
"   (func (export "name") (result i32)
"     nop
"     i32.const 2
"     global.set 0
"     global.get 0)
"   (global (;0;) (mut i32) (i32.const 0))
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAYGAX8BQQALBwgBBG5hbWUAAAoLAQkAAUECJAAjAAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 2 ).
  ENDMETHOD.

ENDCLASS.
