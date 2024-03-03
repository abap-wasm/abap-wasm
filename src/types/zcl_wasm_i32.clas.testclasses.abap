CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.

    METHODS setup.
    METHODS assert_sole_value IMPORTING iv_value TYPE i RAISING zcx_wasm.

    METHODS add FOR TESTING RAISING cx_static_check.
    METHODS sub FOR TESTING RAISING cx_static_check.
    METHODS lt_s FOR TESTING RAISING cx_static_check.
    METHODS div FOR TESTING RAISING cx_static_check.
    METHODS rem_negative1 FOR TESTING RAISING cx_static_check.
    METHODS rem_negative2 FOR TESTING RAISING cx_static_check.
    METHODS rem_negative3 FOR TESTING RAISING cx_static_check.
    METHODS rem_negative4 FOR TESTING RAISING cx_static_check.
    METHODS rem_negative5 FOR TESTING RAISING cx_static_check.
    METHODS div_negative FOR TESTING RAISING cx_static_check.

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
      act = mo_memory->stack_length( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->stack_pop_i32( )->get_signed( )
      exp = iv_value ).

  ENDMETHOD.

  METHOD lt_s.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 3 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 2 ) ).

    zcl_wasm_i32=>lt_s( mo_memory ).

    assert_sole_value( 0 ).

  ENDMETHOD.

  METHOD div.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 5 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 2 ) ).

    zcl_wasm_i32=>div_s( mo_memory ).

    assert_sole_value( 2 ).

  ENDMETHOD.

  METHOD div_negative.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -7 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 3 ) ).
    zcl_wasm_i32=>div_s( mo_memory ).
    assert_sole_value( -2 ).

  ENDMETHOD.

  METHOD rem_negative1.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -5 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 2 ) ).
    zcl_wasm_i32=>rem_s( mo_memory ).
    assert_sole_value( -1 ).

  ENDMETHOD.

  METHOD rem_negative2.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 5 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -2 ) ).
    zcl_wasm_i32=>rem_s( mo_memory ).
    assert_sole_value( 1 ).

  ENDMETHOD.

  METHOD rem_negative3.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -5 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -2 ) ).
    zcl_wasm_i32=>rem_s( mo_memory ).
    assert_sole_value( -1 ).

  ENDMETHOD.

  METHOD rem_negative4.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -7 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 3 ) ).
    zcl_wasm_i32=>rem_s( mo_memory ).
    assert_sole_value( -1 ).

  ENDMETHOD.

  METHOD rem_negative5.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 7 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( -3 ) ).
    zcl_wasm_i32=>rem_s( mo_memory ).
    assert_sole_value( 1 ).

  ENDMETHOD.

  METHOD sub.
* todo, add tests
    RETURN.
  ENDMETHOD.

  METHOD add.

    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 2 ) ).
    mo_memory->stack_push( zcl_wasm_i32=>from_signed( 3 ) ).

    zcl_wasm_i32=>add( mo_memory ).

    assert_sole_value( 5 ).

  ENDMETHOD.

  METHOD test_unsigned_minus_two.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 4294967294 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_signed( )
      exp = -2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 4294967294 ).

  ENDMETHOD.

  METHOD test_unsigned_minus_seven.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 4294967289 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_signed( )
      exp = -7 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 4294967289 ).

  ENDMETHOD.

  METHOD test_unsigned_zero.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_signed( )
      exp = 0 ).

  ENDMETHOD.

  METHOD test_unsigned_one.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_signed( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 1 ).

  ENDMETHOD.

  METHOD test_unsigned_2147483647.

    DATA(lo_int) = zcl_wasm_i32=>from_unsigned( 2147483647 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_signed( )
      exp = 2147483647 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_int->get_unsigned( )
      exp = 2147483647 ).

  ENDMETHOD.

ENDCLASS.
