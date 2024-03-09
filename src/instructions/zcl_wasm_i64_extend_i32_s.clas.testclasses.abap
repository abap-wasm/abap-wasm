CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

" (module
"   (func (export "extend_i32_s") (result i64)
"     i64.const 0x00a0b0c0d0e0f0a0
"     i32.wrap_i64
"     i64.extend_i32_s
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcQAQxleHRlbmRfaTMyX3MAAAoQAQ4AQqDhg4eNmKzQAKesCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'extend_i32_s' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = -790564704 ).
  ENDMETHOD.

ENDCLASS.
