
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_wast.

    METHODS:
      get_first_module FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD get_first_module.

    DATA(lv_wast) =
      |(module\n| &&
      |  (func (export "addTwo") (param i32 i32) (result i32)\n| &&
      |    local.get 0\n| &&
      |    local.get 1\n| &&
      |    i32.add))|.

    mo_cut = NEW #( lv_wast ).

* todo    cl_abap_unit_assert=>assert_not_initial( mo_cut->get_first_module( ) ).

  ENDMETHOD.

ENDCLASS.
