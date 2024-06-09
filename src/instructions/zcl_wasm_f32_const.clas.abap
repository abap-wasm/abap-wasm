CLASS zcl_wasm_f32_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_float TYPE f.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_value TYPE f.
ENDCLASS.

CLASS zcl_wasm_f32_const IMPLEMENTATION.

  METHOD constructor.
* todo, ABAP float is double precision? need single precision?
    mv_value = iv_float.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_f32_const( io_body->shift_f32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    io_memory->mi_stack->push( zcl_wasm_f32=>from_float( mv_value ) ).
  ENDMETHOD.

ENDCLASS.
