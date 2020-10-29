
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_memory TYPE REF TO zcl_wasm_memory.

    METHODS:
      setup,
      assert_sole_value IMPORTING iv_value TYPE i,
      add FOR TESTING,
      sub FOR TESTING,
      lt_s FOR TESTING,
      const_ FOR TESTING.

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
      act = mo_memory->stack_pop_i32( )->get_value( )
      exp = iv_value ).

  ENDMETHOD.

  METHOD lt_s.

    mo_memory->stack_push( NEW zcl_wasm_i32( 3 ) ).
    mo_memory->stack_push( NEW zcl_wasm_i32( 2 ) ).

    zcl_wasm_i32=>lt_s( mo_memory ).

    assert_sole_value( 0 ).

  ENDMETHOD.

  METHOD sub.
* todo, add tests
    RETURN.
  ENDMETHOD.

  METHOD const_.

    CONSTANTS lc_value TYPE i VALUE 42.

    zcl_wasm_i32=>const_( io_memory = mo_memory
                          iv_value  = lc_value ).

    assert_sole_value( lc_value ).

  ENDMETHOD.

  METHOD add.

    mo_memory->stack_push( NEW zcl_wasm_i32( 2 ) ).
    mo_memory->stack_push( NEW zcl_wasm_i32( 3 ) ).

    zcl_wasm_i32=>add( mo_memory ).

    assert_sole_value( 5 ).

  ENDMETHOD.

ENDCLASS.
