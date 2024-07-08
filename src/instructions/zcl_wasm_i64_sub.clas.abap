CLASS zcl_wasm_i64_sub DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_sub IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_sub( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop

    DATA lv_val1 TYPE int8.
    DATA lv_val2 TYPE int8.
    DATA lv_tmp TYPE int8.
    DATA lv_hex1 TYPE x LENGTH 8.
    DATA lv_hex2 TYPE x LENGTH 8.
    DATA lv_result TYPE x LENGTH 8.
    DATA lv_int8 TYPE int8.

    DATA lv_carry TYPE x LENGTH 4.
    DATA lv_borrow TYPE x LENGTH 4.
    DATA lv_word1 TYPE x LENGTH 4.
    DATA lv_word2 TYPE x LENGTH 4.

    DATA lv_negate TYPE abap_bool.

    lv_val1 = io_memory->mi_stack->pop_i64( )->get_signed( ).
    lv_val2 = io_memory->mi_stack->pop_i64( )->get_signed( ).
    IF lv_val1 > lv_val2.
      " WRITE / 'switch'.
      lv_tmp = lv_val1.
      lv_val1 = lv_val2.
      lv_val2 = lv_tmp.
    ELSE.
      lv_negate = abap_true.
    ENDIF.
    lv_hex1 = lv_val1.
    lv_hex2 = lv_val2.
    " WRITE / lv_hex1.
    " WRITE / lv_hex2.

* low 2 bytes
    lv_carry = lv_hex1+6(2) - lv_hex2+6(2).
    lv_result+6(2) = lv_carry+2(2).
    " WRITE / lv_carry.
    IF lv_carry(2) = 'FFFF'.
      lv_carry = '00000001'.
    ELSE.
      lv_carry = '00000000'.
    ENDIF.

* first middle 2 bytes
    lv_carry = lv_hex1+4(2) - lv_hex2+4(2) - lv_carry.
    lv_result+4(2) = lv_carry+2(2).
    " WRITE / lv_carry.
    IF lv_carry(2) = 'FFFF'.
      lv_carry = '00000001'.
    ELSE.
      lv_carry = '00000000'.
    ENDIF.

* second middle 2 bytes
    lv_carry = lv_hex1+2(2) - lv_hex2+2(2) - lv_carry.
    lv_result+2(2) = lv_carry+2(2).
    " WRITE / lv_carry.
    IF lv_carry(2) = 'FFFF'.
      lv_carry = '00000001'.
    ELSE.
      lv_carry = '00000000'.
    ENDIF.

* high 2 bytes
    lv_carry = lv_hex1(2) - lv_hex2(2) - lv_carry.
    lv_result(2) = lv_carry+2(2).

    " WRITE / lv_result.
    lv_int8 = lv_result.
    IF lv_negate = abap_true.
      lv_int8 = lv_int8 * -1.
    ENDIF.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_int8 ) ).

  ENDMETHOD.

ENDCLASS.
