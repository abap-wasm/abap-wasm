CLASS zcl_wast_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !iv_wast         TYPE string
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS func
      IMPORTING
        !io_body           TYPE REF TO zcl_wast_text_stream
      RETURNING
        VALUE(ro_function) TYPE REF TO zcl_wasm_function .
    METHODS instructions
      IMPORTING
        !io_body               TYPE REF TO zcl_wast_text_stream
      RETURNING
        VALUE(rt_instructions) TYPE zcl_wasm_instructions=>ty_instructions .
    METHODS module
      IMPORTING
        !io_body         TYPE REF TO zcl_wast_text_stream
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module .
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
          ro_function = NEW #(
            it_instructions = instructions( io_body ) ).
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD instructions.

    WHILE io_body->get_length( ) > 0.
      DATA(lv_instruction) = io_body->pop( )->peek( ).

      CASE lv_instruction.
* https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
        WHEN 'drop'.
          APPEND zcl_wasm_instructions=>c_instructions-drop TO rt_instructions.
        WHEN 'select'.
          APPEND zcl_wasm_instructions=>c_instructions-select TO rt_instructions.
* https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
        WHEN 'local.get'.
          APPEND zcl_wasm_instructions=>c_instructions-local_get TO rt_instructions.
          io_body->pop( ).
        WHEN 'local.set'.
          APPEND zcl_wasm_instructions=>c_instructions-local_set TO rt_instructions.
          io_body->pop( ).
        WHEN 'local.tee'.
          APPEND zcl_wasm_instructions=>c_instructions-local_tee TO rt_instructions.
          io_body->pop( ).
        WHEN 'global.get'.
          APPEND zcl_wasm_instructions=>c_instructions-global_get TO rt_instructions.
          io_body->pop( ).
        WHEN 'global.set'.
          APPEND zcl_wasm_instructions=>c_instructions-global_set TO rt_instructions.
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

    DATA lt_functions TYPE zcl_wasm_function=>ty_functions.

    WHILE io_body->get_length( ) > 0.
      DATA(lv_next) = io_body->peek( ).

      CASE lv_next.
        WHEN '(func'.
          APPEND func( io_body->pop( ) ) TO lt_functions.
        WHEN OTHERS.
* unknown
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

    ro_module = NEW #( it_functions = lt_functions ).

  ENDMETHOD.


  METHOD parse.

    DATA(lo_text) = NEW zcl_wast_text_stream( iv_wast ).

    WHILE lo_text->get_length( ) > 0.
      DATA(lv_next) = lo_text->peek( ).

      CASE lv_next.
        WHEN '(module'.
          ro_module = module( lo_text->pop( ) ).
* todo, this is not correct, there might be more stuff in the text file
          RETURN.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
