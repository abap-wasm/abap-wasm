CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

" (module
"   (func (export "shl") (result i32)
"   i32.const 0x80000000
"   i32.const 1
"   i32.shl
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNzaGwAAAoNAQsAQYCAgIB4QQF0CwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'shl' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
