CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS two_results FOR TESTING RAISING cx_static_check.

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
      exp = zcl_wasm_types=>c_value_type-i32 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 2 ]->get_type( )
      exp = zcl_wasm_types=>c_value_type-i64 ).

  ENDMETHOD.

ENDCLASS.
