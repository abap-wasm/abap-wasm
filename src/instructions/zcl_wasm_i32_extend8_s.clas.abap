CLASS zcl_wasm_i32_extend8_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_extend8_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_extend8_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-extend-s-mathrm-extend-mathsf-s-m-n-i

    DATA lv_int     TYPE i.
    DATA lv_hex     TYPE x LENGTH 4.
    DATA lv_overlay TYPE x LENGTH 4 VALUE 'FFFFFF00'.

    lv_hex = io_memory->mi_stack->pop_i32( )->mv_value.

    GET BIT 25 OF lv_hex INTO DATA(lv_sign).
    IF lv_sign = 1.
      lv_hex = lv_overlay BIT-OR lv_hex.
    ELSE.
      lv_hex(3) = '000000'.
    ENDIF.

    lv_int = lv_hex.
    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
