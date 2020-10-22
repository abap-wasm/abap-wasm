CLASS zcl_wast_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !iv_wast TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS module
      IMPORTING
        !io_body TYPE REF TO zcl_wast_text_stream .
ENDCLASS.



CLASS ZCL_WAST_PARSER IMPLEMENTATION.


  METHOD module.

    DATA(lv_next) = io_body->peek( ).

    CASE lv_next.
      WHEN '(func'.
* todo
      WHEN OTHERS.
* unknown
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD parse.

    DATA lo_text TYPE REF TO zcl_wast_text_stream.

    lo_text = NEW zcl_wast_text_stream( iv_wast ).

    IF lo_text->peek( ) = '(module'.
      module( lo_text->pop( ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
