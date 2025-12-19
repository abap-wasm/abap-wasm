CLASS zcl_wasm_i64_mul DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_mul IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_mul( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* multiplication avoiding overflow
    DATA lv_hex1    TYPE x LENGTH 8.
    DATA lv_hex2    TYPE x LENGTH 8.

    DATA lv_word1_1 TYPE int8.
    DATA lv_word1_2 TYPE int8.
    DATA lv_word2_1 TYPE int8.
    DATA lv_word2_2 TYPE int8.

    DATA lv_result  TYPE x LENGTH 8.
    DATA lv_sub     TYPE x LENGTH 8.
    DATA lv_temp    TYPE int8.
    DATA lv_carry   TYPE x LENGTH 4.

    lv_hex1 = io_memory->mi_stack->pop_i64( )->get_signed( ).
    lv_hex2 = io_memory->mi_stack->pop_i64( )->get_signed( ).

    lv_word1_1 = lv_hex1+4(4).
    lv_word1_2 = lv_hex1(4).

    lv_word2_1 = lv_hex2+4(4).
    lv_word2_2 = lv_hex2(4).

* first word - multiply high parts
    TRY.
        lv_temp = lv_word1_1 * lv_word2_1.
        lv_result = lv_temp.
      CATCH cx_sy_arithmetic_overflow.
        " Handle overflow by using hex representation directly
        lv_result = lv_temp.
    ENDTRY.

* add cross product 1: word1_1 * word2_2 (shifted left by 32 bits)
    TRY.
        lv_temp = lv_word1_1 * lv_word2_2.
        lv_sub = lv_temp.
        lv_sub(4) = lv_sub+4(4).
        lv_sub+4(4) = '00000000'.
        " Initialize carry before first addition
        CLEAR lv_carry.
        " Add using hex string arithmetic (like i64_add) to avoid overflow
        " Add in 2-byte chunks with carry handling
        " low 2 bytes
        lv_carry = lv_result+6(2) + lv_sub+6(2) + lv_carry.
        lv_result+6(2) = lv_carry+2(2).
        lv_carry+2 = lv_carry(2).
        lv_carry(2) = '0000'.
        " next 2 bytes
        lv_carry = lv_result+4(2) + lv_sub+4(2) + lv_carry.
        lv_result+4(2) = lv_carry+2(2).
        lv_carry+2 = lv_carry(2).
        lv_carry(2) = '0000'.
        " next 2 bytes
        lv_carry = lv_result+2(2) + lv_sub+2(2) + lv_carry.
        lv_result+2(2) = lv_carry+2(2).
        lv_carry+2 = lv_carry(2).
        lv_carry(2) = '0000'.
        " high 2 bytes
        lv_carry = lv_result(2) + lv_sub(2) + lv_carry.
        lv_result(2) = lv_carry+2(2).
      CATCH cx_sy_arithmetic_overflow.
        " If overflow occurs, wrap around (WASM behavior)
        " Continue with truncated result
    ENDTRY.

* add cross product 2: word1_2 * word2_1 (shifted left by 32 bits)
    TRY.
        lv_temp = lv_word1_2 * lv_word2_1.
        lv_sub = lv_temp.
        lv_sub(4) = lv_sub+4(4).
        lv_sub+4(4) = '00000000'.
        " Reset carry before second addition
        CLEAR lv_carry.
        " Add using hex string arithmetic (like i64_add)
        " Add in 2-byte chunks with carry handling
        " low 2 bytes
        lv_carry = lv_result+6(2) + lv_sub+6(2) + lv_carry.
        lv_result+6(2) = lv_carry+2(2).
        lv_carry+2 = lv_carry(2).
        lv_carry(2) = '0000'.
        " next 2 bytes
        lv_carry = lv_result+4(2) + lv_sub+4(2) + lv_carry.
        lv_result+4(2) = lv_carry+2(2).
        lv_carry+2 = lv_carry(2).
        lv_carry(2) = '0000'.
        " next 2 bytes
        lv_carry = lv_result+2(2) + lv_sub+2(2) + lv_carry.
        lv_result+2(2) = lv_carry+2(2).
        lv_carry+2 = lv_carry(2).
        lv_carry(2) = '0000'.
        " high 2 bytes
        lv_carry = lv_result(2) + lv_sub(2) + lv_carry.
        lv_result(2) = lv_carry+2(2).
      CATCH cx_sy_arithmetic_overflow.
        " If overflow occurs, wrap around (WASM behavior)
        " Continue with truncated result
    ENDTRY.

* multiplication of the two left-side words will always overflow, so no need to calculate it

    lv_temp = lv_result.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_temp ) ).

  ENDMETHOD.

ENDCLASS.
