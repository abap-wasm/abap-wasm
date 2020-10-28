
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: list_exports FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD list_exports.

    DATA(lo_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>wasm_add_two( ) ).

    cl_abap_unit_assert=>assert_not_initial( lo_wasm ).

*    DATA(lt_exports) = lo_wasm->list_function_exports( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = lines( lt_exports )
*      exp = 1 ).

  ENDMETHOD.

ENDCLASS.
