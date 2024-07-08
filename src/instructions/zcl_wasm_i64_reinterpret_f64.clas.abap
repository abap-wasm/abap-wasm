CLASS zcl_wasm_i64_reinterpret_f64 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_reinterpret_f64 IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_reinterpret_f64( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA lv_int8 TYPE int8.
    DATA(lv_hex) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) )->get_hex( ).
    lv_int8 = lv_hex.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_int8 ) ).
  ENDMETHOD.

ENDCLASS.
