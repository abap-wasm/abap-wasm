
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS: test FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

    DATA(lo_memory) = NEW zcl_wasm_memory( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_memory->stack_length( )
      exp = 0 ).

    lo_memory->stack_push( zcl_wasm_i32=>from_signed( 2 ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_memory->stack_length( )
      exp = 1 ).

    lo_memory->stack_pop( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_memory->stack_length( )
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
