CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.

    METHODS setup.
    METHODS assert_sole_value IMPORTING iv_value TYPE i RAISING zcx_wasm.

    METHODS test_unsigned_minus_two FOR TESTING RAISING cx_static_check.
    METHODS test_unsigned_minus_seven FOR TESTING RAISING cx_static_check.
    METHODS test_unsigned_zero FOR TESTING RAISING cx_static_check.
    METHODS test_unsigned_one FOR TESTING RAISING cx_static_check.
    METHODS test_unsigned_2147483647 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_memory = NEW zcl_wasm_memory( ).
  ENDMETHOD.

  METHOD assert_sole_value.

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->mi_stack->get_length( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->mi_stack->pop_i32( )->mv_value
      exp = iv_value ).

  ENDMETHOD.

  METHOD test_unsigned_minus_two.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 4294967294 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->mv_value
      exp = -2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 4294967294 ).

  ENDMETHOD.

  METHOD test_unsigned_minus_seven.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 4294967289 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->mv_value
      exp = -7 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 4294967289 ).

  ENDMETHOD.

  METHOD test_unsigned_zero.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->mv_value
      exp = 0 ).

  ENDMETHOD.

  METHOD test_unsigned_one.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->mv_value
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 1 ).

  ENDMETHOD.

  METHOD test_unsigned_2147483647.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 2147483647 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->mv_value
      exp = 2147483647 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 2147483647 ).

  ENDMETHOD.

ENDCLASS.
