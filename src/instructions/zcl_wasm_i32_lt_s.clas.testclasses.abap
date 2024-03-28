CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS no_fold_cmp_s_offset FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD no_fold_cmp_s_offset.

    " (module
    " (func (export "no_fold_cmp_s_offset") (param i32 i32) (result i32)
    "   local.get 0
    "   i32.const 1
    "   i32.add
    "   local.get 1
    "   i32.const 1
    "   i32.add
    "   i32.lt_s))

    DATA(lv_wasm) = `AGFzbQEAAAABBwFgAn9/AX8DAgEABxgBFG5vX2ZvbGRfY21wX3Nfb2Zmc2V0AAAKDwENACAAQQFqIAFBAWpICwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'no_fold_cmp_s_offset'
      it_parameters = VALUE #(
        ( zcl_wasm_i32=>from_signed( 2147483647 ) )
        ( zcl_wasm_i32=>gc_zero ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 1 ).
  ENDMETHOD.

ENDCLASS.
