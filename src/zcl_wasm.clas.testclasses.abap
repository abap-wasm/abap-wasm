
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      add_two FOR TESTING,
      fibonacci FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD add_two.

    DATA lt_values TYPE zif_wasm_value=>ty_values.
    DATA lt_result TYPE zif_wasm_value=>ty_values.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_add_two( ) ).

    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

    DATA(lt_exports) = lo_wasm->list_function_exports( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_exports )
      exp = 1 ).

    APPEND NEW zcl_wasm_i32( 2 ) TO lt_values.
    APPEND NEW zcl_wasm_i32( 3 ) TO lt_values.

    lt_result = lo_wasm->execute_function_export(
      iv_name       = 'add'
      it_parameters = lt_values ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1 ).

* todo, assert lt_result = 5

  ENDMETHOD.

  METHOD fibonacci.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_fibonacci( ) ).

    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

* todo

  ENDMETHOD.

ENDCLASS.
