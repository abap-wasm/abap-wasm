CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS store FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD store.

" (module
"   (func (export "store") (result f64)
"     i32.const 1
"     f64.const 0x00000011
"     f64.store
"     i32.const 1
"     f64.load
"   )
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF8AwIBAAUDAQABBwkBBXN0b3JlAAAKFwEVAEEBRAAAAAAAADFAOQMAQQErAwALAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'store' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_f64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_value( )
      exp = 17 ).
  ENDMETHOD.

ENDCLASS.
