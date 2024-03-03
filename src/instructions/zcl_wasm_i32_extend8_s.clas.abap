CLASS zcl_wasm_i32_extend8_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
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

    DATA lv_hex TYPE x LENGTH 4.
    DATA lv_hex1 TYPE x LENGTH 1.
    DATA lv_ff TYPE x LENGTH 1 VALUE 'FF'.
    DATA lv_int TYPE i.

    lv_hex = io_memory->stack_pop_i32( )->get_signed( ).
    lv_hex1 = lv_hex+3(1).

    GET BIT 1 OF lv_hex1 INTO DATA(lv_sign).
    IF lv_sign = 1.
      lv_hex1 = lv_hex1 BIT-XOR lv_ff.
    ENDIF.

    lv_int = lv_hex1.
    IF lv_sign = 1.
      lv_int = lv_int + 1.
      lv_int = lv_int * -1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
