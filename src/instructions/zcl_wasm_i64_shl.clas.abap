CLASS zcl_wasm_i64_shl DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_shl IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_shl( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ) MOD 64.
    DATA(lv_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).

    DO lv_val1 TIMES.
* todo, handle overflow? do it in binary intead?
      lv_val2 = lv_val2 * 2.
    ENDDO.

    io_memory->stack_push( zcl_wasm_i64=>from_signed( lv_val2 ) ).
  ENDMETHOD.

ENDCLASS.
