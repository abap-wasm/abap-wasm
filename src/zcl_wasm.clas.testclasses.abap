CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS assert_result
      IMPORTING
        it_result TYPE zif_wasm_value=>ty_values
        iv_value  TYPE i.

    METHODS list_function_exports FOR TESTING RAISING cx_static_check.

    METHODS add_two FOR TESTING RAISING cx_static_check.
    METHODS fibonacci FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD list_function_exports.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_add_two( ) ).
    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

    DATA(lt_exports) = lo_wasm->list_function_exports( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_exports )
      exp = 1 ).

  ENDMETHOD.

  METHOD assert_result.

    cl_abap_unit_assert=>assert_equals(
      act = lines( it_result )
      exp = 1 ).

    READ TABLE it_result INDEX 1 INTO DATA(li_data).
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( li_data )->get_value( )
      exp = iv_value ).

  ENDMETHOD.

  METHOD add_two.

    DATA lt_values TYPE zif_wasm_value=>ty_values.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_add_two( ) ).
    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

    APPEND NEW zcl_wasm_i32( 2 ) TO lt_values.
    APPEND NEW zcl_wasm_i32( 3 ) TO lt_values.

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

    APPEND NEW zcl_wasm_i32( 6 ) TO lt_values.

    DATA(lt_result) = lo_wasm->execute_function_export(
      iv_name       = 'fib'
      it_parameters = lt_values ).

    assert_result( it_result = lt_result
                   iv_value  = 13 ).

  ENDMETHOD.

ENDCLASS.
