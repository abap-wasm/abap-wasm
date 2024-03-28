CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS assert_result
      IMPORTING
        it_result TYPE zif_wasm_value=>ty_values
        iv_value  TYPE i.

    METHODS add_two FOR TESTING RAISING cx_static_check.
    METHODS fibonacci FOR TESTING RAISING cx_static_check.
    METHODS factorial FOR TESTING RAISING cx_static_check.
    METHODS malloc1 FOR TESTING RAISING cx_static_check.
    METHODS malloc2 FOR TESTING RAISING cx_static_check.

    METHODS parse_testsuite_i32 FOR TESTING RAISING cx_static_check.
    METHODS parse_testsuite_address FOR TESTING RAISING cx_static_check.
    METHODS parse_testsuite_block FOR TESTING RAISING cx_static_check.
    METHODS parse_testsuite_return FOR TESTING RAISING cx_static_check.
    METHODS parse_testsuite_func FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD assert_result.

    cl_abap_unit_assert=>assert_equals(
      act = lines( it_result )
      exp = 1 ).

    READ TABLE it_result INDEX 1 INTO DATA(li_data).
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( li_data )->mv_value
      exp = iv_value ).

  ENDMETHOD.

  METHOD add_two.

    DATA lt_values TYPE zif_wasm_value=>ty_values.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_add_two( ) ).
    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

    APPEND zcl_wasm_i32=>from_signed( 2 ) TO lt_values.
    APPEND zcl_wasm_i32=>from_signed( 3 ) TO lt_values.

    DATA(lt_result) = lo_wasm->execute_function_export(
      iv_name       = 'add'
      it_parameters = lt_values ).

    assert_result( it_result = lt_result
                   iv_value  = 5 ).

  ENDMETHOD.

  METHOD fibonacci.

    DATA lt_values TYPE zif_wasm_value=>ty_values.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_fibonacci( ) ).
    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

    APPEND zcl_wasm_i32=>from_signed( 6 ) TO lt_values.

    DATA(lt_result) = lo_wasm->execute_function_export(
      iv_name       = 'fib'
      it_parameters = lt_values ).

    assert_result( it_result = lt_result
                   iv_value  = 13 ).

  ENDMETHOD.

  METHOD factorial.

    DATA lt_values TYPE zif_wasm_value=>ty_values.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_factorial( ) ).
    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

    APPEND zcl_wasm_i32=>from_signed( 3 ) TO lt_values.

    DATA(lt_result) = lo_wasm->execute_function_export(
      iv_name       = 'fac'
      it_parameters = lt_values ).

    assert_result( it_result = lt_result
                   iv_value  = 6 ).

  ENDMETHOD.

  METHOD parse_testsuite_i32.
    zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>testsuite_i32( ) ).
  ENDMETHOD.

  METHOD parse_testsuite_address.
    zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>testsuite_address( ) ).
  ENDMETHOD.

  METHOD parse_testsuite_block.
    zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>testsuite_block( ) ).
  ENDMETHOD.

  METHOD parse_testsuite_return.
    zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>testsuite_return( ) ).
  ENDMETHOD.

  METHOD parse_testsuite_func.
    zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>testsuite_func( ) ).
  ENDMETHOD.

  METHOD malloc1.

" (module
"   (func (export "malloc") (param i32) (result i32)
"     (local i32 i32)
"     block  ;; label = @1
"       local.get 0
"       i32.const -4
"       i32.gt_u
"       return
"     end
"     unreachable
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAX8BfwMCAQAHCgEGbWFsbG9jAAAKEAEOAQJ/AkAgAEF8Sw8LAAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'malloc'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( 10 ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 0 ).

  ENDMETHOD.

  METHOD malloc2.

" (module
"   (func (export "malloc") (param i32) (result i32)
"     (local i32 i32)
"     block  ;; label = @1
"       local.get 0
"       i32.const -4
"       i32.gt_u
"       local.tee 1
"       br_if 0 (;@1;)
"       i32.const 123
"       return
"     end
"     unreachable
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBgFgAX8BfwMCAQAHCgEGbWFsbG9jAAAKFwEVAQJ/AkAgAEF8SyIBDQBB+wAPCwALAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'malloc'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( 10 ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = 123 ).

  ENDMETHOD.

ENDCLASS.
