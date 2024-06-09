CLASS zcl_wasm_i32_clz DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_clz IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_clz( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* count leading zeros

    DATA lv_hex TYPE x LENGTH 4.
    DATA lv_bit TYPE i.

    DATA(lv_int) = io_memory->mi_stack->pop_i32( )->mv_value.
    lv_hex = lv_int.

    DATA(lv_zeros) = 0.
    DO 32 TIMES.
      GET BIT sy-index OF lv_hex INTO lv_bit.
      IF lv_bit = 0.
        lv_zeros = lv_zeros + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_zeros ) ).

  ENDMETHOD.

ENDCLASS.
