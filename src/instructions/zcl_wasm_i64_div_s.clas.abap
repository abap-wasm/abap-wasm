CLASS zcl_wasm_i64_div_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_div_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_div_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64.div_s, division by zero'.
    ELSEIF lv_val1 = -1 AND lv_val2 = -9223372036854775808.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64.div_s, signed integer overflow'.
    ENDIF.

* division is truncating, so round towards zero
    IF sign( lv_val1 ) <> sign( lv_val2 ).
      io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( -1 * ( abs( lv_val2 ) DIV abs( lv_val1 ) ) ) ).
    ELSE.
      io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_val2 DIV lv_val1 ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
