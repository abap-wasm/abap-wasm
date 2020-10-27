
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: test1 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test1.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |112233| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->get_length( )
      exp = 3 ).

  ENDMETHOD.

ENDCLASS.
