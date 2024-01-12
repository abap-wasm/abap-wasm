CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS shift_utf8 FOR TESTING RAISING cx_static_check.

    METHODS shift_f32_0 FOR TESTING RAISING cx_static_check.
    METHODS shift_f32_25 FOR TESTING RAISING cx_static_check.

    METHODS shift_f64_0 FOR TESTING RAISING cx_static_check.
    METHODS shift_f64_1 FOR TESTING RAISING cx_static_check.
    METHODS shift_f64_3 FOR TESTING RAISING cx_static_check.
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

  METHOD shift_f32_0.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |00000000| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f32( )
      exp = 0 ).

  ENDMETHOD.

  METHOD shift_f32_25.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |41C80000| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f32( )
      exp = 25 ).

  ENDMETHOD.

  METHOD shift_f64_0.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |0000000000000000| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f64( )
      exp = 0 ).

  ENDMETHOD.

  METHOD shift_f64_1.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |000000000000F03F| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f64( )
      exp = 1 ).

  ENDMETHOD.

  METHOD shift_f64_3.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |0000000000000840| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f64( )
      exp = 3 ).

  ENDMETHOD.

ENDCLASS.
