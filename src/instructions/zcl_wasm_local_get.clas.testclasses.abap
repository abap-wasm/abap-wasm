CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS param_and_locals FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD param_and_locals.

" (module
"   (func (export "loc") (param i32) (result f32)
"     (local f32 f32)
"     local.get 2
" ))

* param is local = 0, and then local 1 and 2 are the f32s

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAX8BfQMCAQAHBwEDbG9jAAAKCAEGAQJ9IAILAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'loc'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( 42 ) ) ) ).

    cl_abap_unit_assert=>assert_not_initial( lt_values ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 1 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-f32 ).

  ENDMETHOD.

ENDCLASS.
