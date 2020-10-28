
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_wasm_parser.

    METHODS:
      setup,
      add_two FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD add_two.

    DATA(lo_module) = mo_cut->parse( zcl_wasm_test_data=>get_add_two( ) ).

    cl_abap_unit_assert=>assert_not_initial( lo_module ).

  ENDMETHOD.

ENDCLASS.
