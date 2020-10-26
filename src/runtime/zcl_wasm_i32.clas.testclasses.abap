
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS: add FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD add.

    DATA(lo_memory) = NEW zcl_wasm_memory( ).

    lo_memory->push( NEW zcl_wasm_i32( 2 ) ).
    lo_memory->push( NEW zcl_wasm_i32( 3 ) ).

    zcl_wasm_i32=>add( lo_memory ).

*    cl_abap_unit_assert=>assert_equals(
*      act = lo_memory->get_length( )
*      exp = 1 ).
*
*    DATA(li_pop) = lo_memory->pop( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = li_pop->get_type( )
*      exp = zcl_wasm_value_types=>c_type-i32 ).
*
*    DATA(lo_int) = CAST zcl_wasm_i32( li_pop ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = lo_int->get_value( )
*      exp = 5 ).

  ENDMETHOD.

ENDCLASS.
