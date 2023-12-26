CLASS zcl_wasm_i32_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_value TYPE i.

    CLASS-METHODS parse
      IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_value TYPE i.
ENDCLASS.

CLASS zcl_wasm_i32_const IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_i32_const( io_body->shift_int( ) ).
  ENDMETHOD.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>const_(
      io_memory = io_memory
      iv_value  = mv_value ).
  ENDMETHOD.

ENDCLASS.