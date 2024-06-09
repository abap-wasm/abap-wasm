CLASS zcl_wasm_i64_ctz DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_ctz IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_ctz( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* count trailing zeros
    DATA lv_zeros TYPE int8.
    DATA lv_bit TYPE i.
    DATA lv_hex TYPE x LENGTH 8.

    lv_hex = io_memory->mi_stack->pop_i64( )->get_signed( ).

    DO 64 TIMES.
      DATA(lv_index) = 64 - sy-index + 1.
      GET BIT lv_index OF lv_hex INTO lv_bit.
      IF lv_bit = 0.
        lv_zeros = lv_zeros + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_zeros ) ).

  ENDMETHOD.

ENDCLASS.
