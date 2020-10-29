
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_wast_parser.

    METHODS:
      setup,
      parse_add_two FOR TESTING,
      parse_fib FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD parse_add_two.

    DATA(lv_wast) =
      |(module\n| &&
      |  (func (export "addTwo") (param i32 i32) (result i32)\n| &&
      |    (local.get 0)\n| &&
      |    (local.get 1)\n| &&
      |    (i32.add )))|.

    DATA(lo_module) = mo_cut->parse( lv_wast ).

    cl_abap_unit_assert=>assert_not_initial( lo_module ).

* todo,
*    DATA(lt_functions) = lo_module->get_functions( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = lines( lt_functions )
*      exp = 1 ).
*
*    READ TABLE lt_functions INDEX 1 INTO DATA(lo_function).
*    cl_abap_unit_assert=>assert_not_initial( lo_function ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = lines( lo_function->get_instructions( ) )
*      exp = 3 ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = lo_function->get_export_name( )
*      exp = |addTwo| ).

  ENDMETHOD.

  METHOD parse_fib.

    DATA(lv_wast) =
      |(module\n| &&
      | (export "fib" (func $fib))\n| &&
      | (func $fib (param $n i32) (result i32)\n| &&
      |  (if\n| &&
      |   (i32.lt_s\n| &&
      |    (local.get $n)\n| &&
      |    (i32.const 2)\n| &&
      |   )\n| &&
      |   (return\n| &&
      |    (i32.const 1)\n| &&
      |   )\n| &&
      |  )\n| &&
      |  (return\n| &&
      |   (i32.add\n| &&
      |    (call $fib\n| &&
      |     (i32.sub\n| &&
      |      (local.get $n)\n| &&
      |      (i32.const 2)\n| &&
      |     )\n| &&
      |    )\n| &&
      |    (call $fib\n| &&
      |     (i32.sub\n| &&
      |      (local.get $n)\n| &&
      |      (i32.const 1)\n| &&
      |     )\n| &&
      |    )\n| &&
      |   )\n| &&
      |  )\n| &&
      | )\n| &&
      |)\n|.

    DATA(lo_module) = mo_cut->parse( lv_wast ).

* todo,
*    cl_abap_unit_assert=>assert_not_initial( lo_module ).

  ENDMETHOD.

ENDCLASS.
