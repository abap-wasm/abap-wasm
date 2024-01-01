CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS shift_int FOR TESTING RAISING cx_static_check.
    METHODS shift_utf8 FOR TESTING RAISING cx_static_check.
    METHODS shift_f32_25 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test1.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |112233| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->get_length( )
      exp = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->peek( 1 )
      exp = |11| ).

    lo_stream->shift( 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->get_length( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->peek( 2 )
      exp = |2233| ).

  ENDMETHOD.

  METHOD shift_utf8.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |03616263| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_utf8( )
      exp = |abc| ).

  ENDMETHOD.

  METHOD shift_int.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |02| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_int( )
      exp = 2 ).

  ENDMETHOD.

  METHOD shift_f32_25.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |41C80000| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f32( )
      exp = 25 ).

  ENDMETHOD.

ENDCLASS.
