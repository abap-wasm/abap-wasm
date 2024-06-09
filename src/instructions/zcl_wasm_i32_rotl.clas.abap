CLASS zcl_wasm_i32_rotl DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING
        !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_rotl IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_rotl( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-irotl-mathrm-irotl-n-i-1-i-2

    DATA lv_hex    TYPE x LENGTH 4.
    DATA lv_char32 TYPE c LENGTH 32.
    DATA lv_int    TYPE i.
    DATA lv_offset TYPE i.

    DATA(li_stack) = io_memory->mi_stack.
    DATA(lv_bits) = li_stack->pop_i32( )->mv_value MOD 32.
    lv_hex = li_stack->pop_i32( )->mv_value.

    DATA(lv_bytes) = lv_bits DIV 8.
    lv_bits = lv_bits MOD 8.

    IF lv_bits = 0.
      SHIFT lv_hex LEFT BY lv_bytes PLACES IN BYTE MODE CIRCULAR.
    ELSE.
      lv_bits = lv_bits + lv_bytes * 8.

      DO 32 TIMES.
        lv_offset = sy-index - 1.
        GET BIT sy-index OF lv_hex INTO lv_int.
        lv_char32+lv_offset(1) = lv_int.
      ENDDO.

      SHIFT lv_char32 LEFT BY lv_bits PLACES CIRCULAR.

      DO 32 TIMES.
        lv_offset = sy-index - 1.
        lv_int = lv_char32+lv_offset(1).
        SET BIT sy-index OF lv_hex TO lv_int.
      ENDDO.
    ENDIF.

    lv_int = lv_hex.
    li_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).

  ENDMETHOD.

ENDCLASS.
