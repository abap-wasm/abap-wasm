CLASS zcl_wasm_i64_add DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_add IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_add( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* wasm does C style overflows, but ABAP dumps, so split up the addition into multiple safe parts
* addition of 2 bytes at a time, arithmetics are typically 4 bytes in ABAP

    DATA lv_hex1   TYPE x LENGTH 8.
    DATA lv_hex2   TYPE x LENGTH 8.
    DATA lv_result TYPE x LENGTH 8.
    DATA lv_int8   TYPE int8.
    DATA lv_carry  TYPE x LENGTH 4.

    lv_hex1 = io_memory->mi_stack->pop_i64( )->get_signed( ).
    lv_hex2 = io_memory->mi_stack->pop_i64( )->get_signed( ).

* low 2 bytes
    lv_carry = lv_hex1+6(2) + lv_hex2+6(2) + lv_carry.
    lv_result+6(2) = lv_carry+2(2).
    lv_carry+2 = lv_carry(2).
    lv_carry(2) = '0000'.

* first middle 2 bytes
    lv_carry = lv_hex1+4(2) + lv_hex2+4(2) + lv_carry.
    lv_result+4(2) = lv_carry+2(2).
    lv_carry+2 = lv_carry(2).
    lv_carry(2) = '0000'.

* second middle 2 bytes
    lv_carry = lv_hex1+2(2) + lv_hex2+2(2) + lv_carry.
    lv_result+2(2) = lv_carry+2(2).
    lv_carry+2 = lv_carry(2).
    lv_carry(2) = '0000'.

* high 2 bytes
    lv_carry = lv_hex1(2) + lv_hex2(2) + lv_carry.
    lv_result(2) = lv_carry+2(2).

    lv_int8 = lv_result.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_int8 ) ).

  ENDMETHOD.

ENDCLASS.
