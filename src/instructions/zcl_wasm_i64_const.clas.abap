CLASS zcl_wasm_i64_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_value TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_value TYPE int8.
ENDCLASS.

CLASS zcl_wasm_i64_const IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD parse.
* todo: singletons?
    DATA(lv_value) = io_body->shift_i64( ).
    ri_instruction = NEW zcl_wasm_i64_const( lv_value ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_i64_const' ).
  ENDMETHOD.

ENDCLASS.
