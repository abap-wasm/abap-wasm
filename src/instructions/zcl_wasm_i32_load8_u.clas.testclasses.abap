CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.
    METHODS negative_input FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD empty.

    " (module
    " (type (;0;) (func (param i32) (result i32)))
    " (func (;1;) (type 0) (param i32) (result i32)
    "   local.get 0
    "   i32.load8_u)
    " (memory (;0;) 1 1)
    " (export "memory0" (memory 0))
    " (export "load8_u" (func 0))
    " (data (;0;) (i32.const 2) "\03\01\04\01")
    " (data (;1;) (i32.const 12) "\07\05\02\03\06"))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAX8BfwMCAQAFBAEBAQEHFQIHbWVtb3J5MAIAB2xvYWQ4X3UAAAoJAQcAIAAtAAALCxQCAEECCwQDAQQBAEEMCwUHBQIDBgAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'load8_u'
      it_parameters = VALUE #( ( zcl_wasm_i32=>gc_zero ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 0 ).
  ENDMETHOD.

  METHOD negative_input.

" (module
"   (func (export "name") (result i32)
"     i32.const 0
"     i32.const -1
"     i32.store
"     i32.const 0
"     i32.load8_u)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBwgBBG5hbWUAAAoQAQ4AQQBBfzYCAEEALQAACwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 255 ).

  ENDMETHOD.

ENDCLASS.
