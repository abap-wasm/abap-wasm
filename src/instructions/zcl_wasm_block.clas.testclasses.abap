CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD empty.
    DATA(li_wasm) = zcl_wasm=>create_with_wasm( zcl_wasm_test_data=>testsuite_block( ) ).

*    li_wasm->execute_function_export( 'empty' ).
  ENDMETHOD.

ENDCLASS.
