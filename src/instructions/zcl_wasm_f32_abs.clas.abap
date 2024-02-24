CLASS zcl_wasm_f32_abs DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_f32_abs IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_f32_abs( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).
    io_memory->stack_push( zcl_wasm_f32=>from_float( abs( lo_val1->get_value( ) ) ) ).
  ENDMETHOD.

ENDCLASS.
