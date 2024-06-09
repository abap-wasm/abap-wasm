CLASS zcl_wasm_i32_xor DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_xor IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_xor( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA lv_hex1 TYPE x LENGTH 4.
    DATA lv_hex2 TYPE x LENGTH 4.
    DATA lv_int  TYPE i.

    DATA(li_stack) = io_memory->mi_stack.
    lv_hex1 = CAST zcl_wasm_i32( li_stack->pop( ) )->mv_value.
    lv_hex2 = CAST zcl_wasm_i32( li_stack->pop( ) )->mv_value.

    lv_hex1 = lv_hex1 BIT-XOR lv_hex2.
    lv_int = lv_hex1.

    li_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
