CLASS zcl_wasm_i32_wrap_i64 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_wrap_i64 IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_wrap_i64( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-wrap-mathrm-wrap-m-n-i

    DATA lv_int8 TYPE int8.
    DATA lv_int4 TYPE i.
    DATA lv_hex8 TYPE x LENGTH 8.
    DATA lv_hex4 TYPE x LENGTH 4.

    lv_int8 = io_memory->mi_stack->pop_i64( )->get_signed( ).
    lv_hex8 = lv_int8.
    lv_hex4 = lv_hex8+4(4).
    lv_int4 = lv_hex4.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_int4 ) ).
  ENDMETHOD.

ENDCLASS.
