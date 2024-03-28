CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS two_results FOR TESTING RAISING cx_static_check.
    METHODS multi_value FOR TESTING RAISING cx_static_check.
    METHODS call_and_br FOR TESTING RAISING cx_static_check.
    METHODS call_and_br_after FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD two_results.

    " (module
    " (func (export "twoResults") (result i32 i64)
    "   i32.const 1
    "   i64.const 2))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAAJ/fgMCAQAHDgEKdHdvUmVzdWx0cwAACggBBgBBAUICCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'twoResults' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 1 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-i32 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 2 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-i64 ).

  ENDMETHOD.

  METHOD multi_value.

    " (module
    " (func $swap (param i32 i32) (result i32 i32)
    "   local.get 1
    "   local.get 0)
    " (func (export "reverseSub") (param i32 i32) (result i32)
    "   local.get 0
    "   local.get 1
    "   call $swap
    "   i32.sub))

    DATA(lv_wasm) = `AGFzbQEAAAABDgJgAn9/An9/YAJ/fwF/AwMCAAEHDgEKcmV2ZXJzZVN1YgABChICBgAgASAACwkAIAAgARAAawsAFQRuYW1lAQcBAARzd2FwAgUCAAABAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'reverseSub'
      it_parameters = VALUE #(
        ( zcl_wasm_i32=>from_signed( 10 ) )
        ( zcl_wasm_i32=>from_signed( 3 ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 1 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-i32 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = -7 ).

  ENDMETHOD.

  METHOD call_and_br.

" (module
"   (func $moo
"     block (result i32)
"       i32.const 1
"       br 1
"     end
"     drop)
"   (func (export "foo") (result i32)
"     i32.const 123
"     call $moo)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABCAJgAABgAAF/AwMCAAEHBwEDZm9vAAEKFAIKAAJ/QQEMAQsaCwcAQfsAEAALABQEbmFtZQEGAQADbW9vAgUCAAABAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'foo' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 123 ).

  ENDMETHOD.

  METHOD call_and_br_after.

" (module
"   (func $moo
"     block (result i32)
"       br 1
"       i32.const 1
"     end
"     drop)
"   (func (export "foo") (result i32)
"     i32.const 123
"     call $moo)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABCAJgAABgAAF/AwMCAAEHBwEDZm9vAAEKFAIKAAJ/DAFBAQsaCwcAQfsAEAALABQEbmFtZQEGAQADbW9vAgUCAAABAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'foo' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 123 ).

  ENDMETHOD.

ENDCLASS.
