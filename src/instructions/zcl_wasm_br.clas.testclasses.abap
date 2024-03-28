CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS asreturnvalues FOR TESTING RAISING cx_static_check.
    METHODS unwind_func FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD asreturnvalues.

" (module
"   (func (export "asreturnvalues") (result i32 i64)
"     i32.const 2
"     block (result i64)  ;; label = @1
"       i32.const 1
"       i64.const 7
"       br 0 (;@1;)
"       return
"     end
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAAJ/fgMCAQAHEgEOYXNyZXR1cm52YWx1ZXMAAAoQAQ4AQQICfkEBQgcMAA8LCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'asreturnvalues' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 1 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-i32 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 2 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-i64 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 2 ] )->get_signed( )
      exp = 7 ).

  ENDMETHOD.

  METHOD unwind_func.

" (module
"   (func (export "unwind") (result i32)
"     i32.const 3
"     i64.const 1
"     i32.const 9
"     br 0 (;@0;)
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZ1bndpbmQAAAoMAQoAQQNCAUEJDAALAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'unwind' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 9 ).

  ENDMETHOD.

ENDCLASS.
