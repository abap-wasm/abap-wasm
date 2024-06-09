CLASS zcl_wasm_i64_rem_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_rem_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_rem_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    DATA lv_result TYPE int8.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64.rem_s, division by zero'.
    ENDIF.

    lv_result = abs( lv_val2 ) MOD abs( lv_val1 ).
    IF lv_val2 < 0.
      lv_result = lv_result * -1.
    ENDIF.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_result ) ).

  ENDMETHOD.

ENDCLASS.
