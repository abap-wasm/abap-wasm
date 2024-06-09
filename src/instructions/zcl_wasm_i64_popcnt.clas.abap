CLASS zcl_wasm_i64_popcnt DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_popcnt IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_popcnt( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* Return the count of non-zero bits in i

    DATA lv_hex TYPE x LENGTH 8.
    DATA lv_bit TYPE i.
    DATA lv_count TYPE int8.

    lv_hex = io_memory->mi_stack->pop_i64( )->get_signed( ).

    lv_count = 0.
    DO 64 TIMES.
      GET BIT sy-index OF lv_hex INTO lv_bit.
      IF lv_bit = 1.
        lv_count = lv_count + 1.
      ENDIF.
    ENDDO.

    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_count ) ).
  ENDMETHOD.

ENDCLASS.
