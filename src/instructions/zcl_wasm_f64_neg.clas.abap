CLASS zcl_wasm_f64_neg DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_f64_neg IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_f64_neg( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA(lv_float) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) )->get_value( ).
    lv_float = -1 * lv_float.
    io_memory->mi_stack->push( zcl_wasm_f64=>from_float( lv_float ) ).
  ENDMETHOD.

ENDCLASS.
