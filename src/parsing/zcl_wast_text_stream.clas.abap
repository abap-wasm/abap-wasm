CLASS zcl_wast_text_stream DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_text TYPE string .
    METHODS get_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS get_text
      RETURNING
        VALUE(rv_text) TYPE string .
    METHODS peek
      RETURNING
        VALUE(rv_text) TYPE string .
    METHODS pop
      RETURNING
        VALUE(ro_body) TYPE REF TO zcl_wast_text_stream .
  PROTECTED SECTION.

    METHODS remove_comments
      IMPORTING
        !iv_text       TYPE string
      RETURNING
        VALUE(rv_text) TYPE string .
    METHODS find_match_paren
      RETURNING
        VALUE(rv_index) TYPE i .
  PRIVATE SECTION.

    DATA mv_text TYPE string .
ENDCLASS.



CLASS zcl_wast_text_stream IMPLEMENTATION.


  METHOD constructor.

    mv_text = remove_comments( iv_text ).

    REPLACE ALL OCCURRENCES OF |\n| IN mv_text WITH | |.

    CONDENSE mv_text.

  ENDMETHOD.


  METHOD find_match_paren.

* todo, this does not take strings into account

    DATA(lv_counter) = 0.
    DATA(lv_length) = strlen( mv_text ).

    DO lv_length TIMES.
      DATA(lv_offset) = sy-index - 1.

      DATA(lv_current) = mv_text+lv_offset(1).
      IF lv_current = '('.
        lv_counter = lv_counter + 1.
      ELSEIF lv_current = ')'.
        lv_counter = lv_counter - 1.
        IF lv_counter = 0.
          rv_index = lv_offset.
          RETURN.
        ENDIF.
      ENDIF.

    ENDDO.

* no matching paren found
    ASSERT 0 = 1.

  ENDMETHOD.


  METHOD get_length.

    rv_length = strlen( mv_text ).

  ENDMETHOD.


  METHOD get_text.
    rv_text = mv_text.
  ENDMETHOD.


  METHOD peek.

    DATA lt_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    SPLIT mv_text AT | | INTO TABLE lt_table.

    READ TABLE lt_table INDEX 1 INTO rv_text.

  ENDMETHOD.


  METHOD pop.

    DATA(lv_peek) = peek( ).

    DATA(lv_offset) = strlen( lv_peek ).

    IF lv_peek(1) = '('.
      DATA(lv_paren) = find_match_paren( ).
      DATA(lv_body_length) = lv_paren - lv_offset.

      IF lv_body_length > 0.
        DATA(lv_new) = mv_text+lv_offset(lv_body_length).
        ro_body = NEW #( lv_new ).
      ENDIF.

      lv_paren = lv_paren + 1.
      mv_text = mv_text+lv_paren.
    ELSE.
      DATA(lv_text) = mv_text(lv_offset).
      ro_body = NEW #( lv_text ).
      mv_text = mv_text+lv_offset.
    ENDIF.

    CONDENSE mv_text.

  ENDMETHOD.


  METHOD remove_comments.

    SPLIT iv_text AT |\n| INTO TABLE DATA(lt_rows).

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<lv_row>).
      FIND FIRST OCCURRENCE OF |;;| IN <lv_row> MATCH OFFSET DATA(lv_offset).
      IF sy-subrc = 0.
        <lv_row> = <lv_row>(lv_offset).
      ENDIF.
    ENDLOOP.

    rv_text = concat_lines_of( table = lt_rows
                               sep   = |\n| ).

  ENDMETHOD.
ENDCLASS.
