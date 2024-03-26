CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS shift_utf8 FOR TESTING RAISING cx_static_check.

    METHODS shift_f32_0 FOR TESTING RAISING cx_static_check.
    METHODS shift_f32_25 FOR TESTING RAISING cx_static_check.

    METHODS shift_f64_0 FOR TESTING RAISING cx_static_check.
    METHODS shift_f64_1 FOR TESTING RAISING cx_static_check.
    METHODS shift_f64_3 FOR TESTING RAISING cx_static_check.

    METHODS shift_i32_minus2 FOR TESTING RAISING cx_static_check.
    METHODS shift_i32_minus3 FOR TESTING RAISING cx_static_check.
    METHODS shift_i32_minusmany FOR TESTING RAISING cx_static_check.
    METHODS shift_i32_n2072745074 FOR TESTING RAISING cx_static_check.

    METHODS shift_i64_0 FOR TESTING RAISING cx_static_check.
    METHODS shift_i64_1 FOR TESTING RAISING cx_static_check.
    METHODS shift_i64_n1 FOR TESTING RAISING cx_static_check.
    METHODS shift_i64_max_positive FOR TESTING RAISING cx_static_check.
    METHODS shift_i64_max_negative FOR TESTING RAISING cx_static_check.
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

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |0000C841| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f32( )
      exp = 25 ).

  ENDMETHOD.

  METHOD shift_f64_0.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |0000000000000000| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f64( )->get_value( )
      exp = 0 ).

  ENDMETHOD.

  METHOD shift_f64_1.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |000000000000F03F| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f64( )->get_value( )
      exp = 1 ).

  ENDMETHOD.

  METHOD shift_f64_3.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |0000000000000840| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_f64( )->get_value( )
      exp = 3 ).

  ENDMETHOD.

  METHOD shift_i32_minus2.
* 0x7E = 1111110
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |7E| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i32( )
      exp = -2 ).
  ENDMETHOD.

  METHOD shift_i32_minus3.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |7D| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i32( )
      exp = -3 ).
  ENDMETHOD.

  METHOD shift_i32_minusmany.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |C0BB78| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i32( )
      exp = -123456 ).
  ENDMETHOD.

  METHOD shift_i32_n2072745074.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |8ED7D1A378| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i32( )
      exp = -2072745074 ).
  ENDMETHOD.

  METHOD shift_i64_0.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |00| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i64( )
      exp = 0 ).
  ENDMETHOD.

  METHOD shift_i64_1.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |01| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i64( )
      exp = 1 ).
  ENDMETHOD.

  METHOD shift_i64_n1.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |7F| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i64( )
      exp = -1 ).
  ENDMETHOD.

  METHOD shift_i64_max_positive.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |FFFFFFFFFFFFFFFFFF00| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i64( )
      exp = 9223372036854775807 ).
  ENDMETHOD.

  METHOD shift_i64_max_negative.
    DATA(lo_stream) = NEW zcl_wasm_binary_stream( CONV xstring( |8080808080808080807F| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->shift_i64( )
      exp = -9223372036854775808 ).
  ENDMETHOD.

ENDCLASS.
