CLASS zcl_wasm_i64_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_value TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_value TYPE int8.
ENDCLASS.

CLASS zcl_wasm_i64_const IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD parse.
    DATA(lv_value) = io_body->shift_i64( ).
    ri_instruction = NEW zcl_wasm_i64_const( lv_value ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( mv_value ) ).
  ENDMETHOD.

ENDCLASS.
