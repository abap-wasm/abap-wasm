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

    DATA lv_result  TYPE int8.
    DATA lv_sub     TYPE x LENGTH 8.

    lv_hex1 = io_memory->mi_stack->pop_i64( )->get_signed( ).
    lv_hex2 = io_memory->mi_stack->pop_i64( )->get_signed( ).

    lv_word1_1 = lv_hex1+4(4).
    lv_word1_2 = lv_hex1(4).

    lv_word2_1 = lv_hex2+4(4).
    lv_word2_2 = lv_hex2(4).

* first word
    lv_result = lv_word1_1 * lv_word2_1.

    lv_sub = lv_word1_1 * lv_word2_2.
    lv_sub(4) = lv_sub+4(4).
    lv_sub+4(4) = '00000000'.
    lv_result = lv_result + lv_sub.

* second word
    lv_sub = lv_word1_2 * lv_word2_1.
    lv_sub(4) = lv_sub+4(4).
    lv_sub+4(4) = '00000000'.
    lv_result = lv_result + lv_sub.

* multiplication of the two left-side words will always overflow, so no need to calculate it

    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_result ) ).

  ENDMETHOD.

ENDCLASS.
