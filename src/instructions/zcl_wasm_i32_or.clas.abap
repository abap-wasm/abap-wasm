CLASS zcl_wasm_i32_or DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_or IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_or( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA lv_hex1 TYPE x LENGTH 4.
    DATA lv_hex2 TYPE x LENGTH 4.

    "##feature-start=debug
    ASSERT io_memory->mi_stack->get_length( ) >= 2.
    "##feature-end=debug

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->mi_stack->pop( ) )->mv_value.
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->mi_stack->pop( ) )->mv_value.
    lv_hex1 = lv_val1.
    lv_hex2 = lv_val2.

    lv_hex1 = lv_hex1 BIT-OR lv_hex2.
    lv_val1 = lv_hex1.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_val1 ) ).
  ENDMETHOD.

ENDCLASS.
