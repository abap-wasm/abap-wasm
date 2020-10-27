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

      IF lv_next(1) = '$'.
        io_body->pop( ).
        DATA(lv_id) = lv_next.
        CONTINUE.
      ENDIF.

      CASE lv_next.
        WHEN '(export'.
          DATA(lv_export_name) = io_body->pop( )->peek( ).
          REPLACE ALL OCCURRENCES OF '"' IN lv_export_name WITH ||.
        WHEN '(param'.
          DATA(lv_p) = io_body->pop( ).
        WHEN '(result'.
          DATA(lv_r) = io_body->pop( ).
        WHEN OTHERS.
          ro_function = NEW #(
* todo, iv_id = lv_id
            iv_export_name  = lv_export_name
            it_instructions = instructions( io_body ) ).
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD instructions.

    WHILE io_body->get_length( ) > 0.
      DATA(lv_instruction) = io_body->peek( ).
      io_body->pop( ).

      CASE lv_instruction.
* https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
        WHEN '(drop'.
          APPEND zcl_wasm_instructions=>c_instructions-drop TO rt_instructions.
        WHEN '(select'.
          APPEND zcl_wasm_instructions=>c_instructions-select TO rt_instructions.
* https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
        WHEN '(local.get'.
          APPEND zcl_wasm_instructions=>c_instructions-local_get TO rt_instructions.
        WHEN '(local.set'.
          APPEND zcl_wasm_instructions=>c_instructions-local_set TO rt_instructions.
        WHEN '(local.tee'.
          APPEND zcl_wasm_instructions=>c_instructions-local_tee TO rt_instructions.
        WHEN '(global.get'.
          APPEND zcl_wasm_instructions=>c_instructions-global_get TO rt_instructions.
        WHEN '(global.set'.
          APPEND zcl_wasm_instructions=>c_instructions-global_set TO rt_instructions.
* https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
* todo
* https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
        WHEN '(i32.const'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.const'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.const'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.const'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.eqz'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.eq'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.ne'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.lt_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.lt_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.gt_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.gt_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.le_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.le_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.ge_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.ge_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.eqz'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.eq'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.ne'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.lt_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.lt_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.gt_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.gt_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.le_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.le_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.ge_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i64.ge_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.eq'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.ne'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.lt'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.gt'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.le'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f32.ge'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.eq'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.ne'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.lt'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.gt'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.le'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(f64.ge'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.clz'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.ctz'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.popcnt'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.add'.
          APPEND zcl_wasm_instructions=>c_instructions-i32_add TO rt_instructions.
        WHEN '(i32.sub'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.mul'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.div_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.div_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.rem_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.rem_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.and'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.or'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.xor'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.shl'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.shr_s'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.shr_u'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.rotl'.
* todo,          APPEND zcl_wasm_instructions=>c_instructions- TO rt_instructions.
        WHEN '(i32.rotr'.
* todo, more numeric instructions
        WHEN '(if'.
* todo
        WHEN '(return'.
* todo
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
        WHEN '(export'.
* todo
          io_body->pop( ).
        WHEN OTHERS.
* unknown
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

    ro_module = NEW #(
* todo, it_exports = lt_exports
      it_functions = lt_functions ).

  ENDMETHOD.


  METHOD parse.

    DATA(lo_text) = NEW zcl_wast_text_stream( iv_wast ).

    WHILE lo_text->get_length( ) > 0.
      DATA(lv_next) = lo_text->peek( ).

      CASE lv_next.
        WHEN '(module'.
          ro_module = module( lo_text->pop( ) ).
* todo, this is not correct, there might be more stuff in the text file ?
          RETURN.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
