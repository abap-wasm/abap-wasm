
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_wast_text_stream.

    METHODS:
      single_module FOR TESTING,
      instructions FOR TESTING,
      comments FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD single_module.

    DATA(lo_text) = NEW zcl_wast_text_stream( '(module body)' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_text->get_length( )
      exp = 13 ).

    cl_abap_unit_assert=>assert_equals(
       act = lo_text->peek( )
       exp = '(module' ).

    DATA(lo_body) = lo_text->pop( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_body->get_text( )
      exp = 'body' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_text->get_length( )
      exp = 0 ).

  ENDMETHOD.

  METHOD instructions.

    DATA(lo_text) = NEW zcl_wast_text_stream( 'foo bar' ).

    cl_abap_unit_assert=>assert_equals(
       act = lo_text->peek( )
       exp = 'foo' ).

    DATA(lo_pop) = lo_text->pop( ).

    cl_abap_unit_assert=>assert_not_initial( lo_pop ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_text->peek( )
      exp = 'bar' ).

  ENDMETHOD.

  METHOD comments.

    DATA(lo_text) = NEW zcl_wast_text_stream( | ;;foo \n bar| ).

    cl_abap_unit_assert=>assert_equals(
       act = lo_text->pop( )->peek( )
       exp = 'bar' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_text->get_length( )
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
