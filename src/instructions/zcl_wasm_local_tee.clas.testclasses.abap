CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.
    METHODS something FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD empty.

    " (module
    " (func (export "tee") (result i32)
    "   (local i32)
    "   i32.const 0
    "   local.tee 0))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQN0ZWUAAAoKAQgBAX9BACIACwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'tee' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

* expected value = 0
  ENDMETHOD.

  METHOD something.

" (module
"   (func (export "something") (result i32)
"     (local i32)
"     i32.const 1
"     i32.load
"     local.tee 0
"     i32.const 1
"     i32.load
"     i32.eq
"   )
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBw0BCXNvbWV0aGluZwAAChMBEQEBf0EBKAIAIgBBASgCAEYLAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'something' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.
