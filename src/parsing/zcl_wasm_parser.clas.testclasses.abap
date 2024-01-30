CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_wasm_parser.

    METHODS:
      setup,
      add_two FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD add_two.

    DATA(lo_module) = mo_cut->parse( zcl_wasm_test_data=>wasm_add_two( ) ).

    cl_abap_unit_assert=>assert_not_initial( lo_module ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_module->get_codes( ) )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_module->get_exports( ) )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_module->get_functions( ) )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_module->get_types( ) )
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.
