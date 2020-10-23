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

    METHODS func
      IMPORTING
        !io_body TYPE REF TO zcl_wast_text_stream .
    METHODS instructions
      IMPORTING
        !io_body TYPE REF TO zcl_wast_text_stream .
    METHODS module
      IMPORTING
        !io_body TYPE REF TO zcl_wast_text_stream .
ENDCLASS.



CLASS ZCL_WAST_PARSER IMPLEMENTATION.


  METHOD func.

* https://webassembly.github.io/spec/core/text/modules.html#functions

    WHILE io_body->get_length( ) > 0.
      DATA(lv_next) = io_body->peek( ).

      CASE lv_next.
        WHEN '(export'.
          DATA(lv_e) = io_body->pop( ).
        WHEN '(param'.
          DATA(lv_p) = io_body->pop( ).
        WHEN '(result'.
          DATA(lv_r) = io_body->pop( ).
        WHEN OTHERS.
          instructions( io_body ).
          RETURN.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD instructions.


    WHILE io_body->get_length( ) > 0.
      DATA(lv_instruction) = io_body->pop( )->peek( ).

      CASE lv_instruction.
* https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
        WHEN 'drop'.
        WHEN 'select'.
* https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
        WHEN 'local.get'.
          io_body->pop( ).
        WHEN 'local.set'.
          io_body->pop( ).
        WHEN 'local.tee'.
          io_body->pop( ).
        WHEN 'global.get'.
          io_body->pop( ).
        WHEN 'global.set'.
          io_body->pop( ).
* https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
* todo
        WHEN 'i32.add'.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD module.

    DATA(lv_next) = io_body->peek( ).

    CASE lv_next.
      WHEN '(func'.
        func( io_body->pop( ) ).
      WHEN OTHERS.
* unknown
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD parse.

    DATA(lo_text) = NEW zcl_wast_text_stream( iv_wast ).

    IF lo_text->peek( ) = '(module'.
      module( lo_text->pop( ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
