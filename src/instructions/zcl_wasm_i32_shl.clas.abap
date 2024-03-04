CLASS zcl_wasm_i32_shl DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_shl IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_shl( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-ishl-mathrm-ishl-n-i-1-i-2

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ) MOD 32.
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    DO lv_val1 TIMES.
      lv_val2 = lv_val2 * 2.
    ENDDO.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_val2 ) ).
  ENDMETHOD.

ENDCLASS.
