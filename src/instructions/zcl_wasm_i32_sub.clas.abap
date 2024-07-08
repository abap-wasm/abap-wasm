CLASS zcl_wasm_i32_sub DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_sub IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_sub( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop

    DATA lv_val1 TYPE int8.
    DATA lv_val2 TYPE int8.
    DATA lv_int TYPE i.

    lv_val1 = io_memory->mi_stack->pop_i32( )->mv_value.
    lv_val2 = io_memory->mi_stack->pop_i32( )->mv_value.

    lv_val1 = lv_val2 - lv_val1.

    lv_val1 = lv_val1 MOD 4294967296.
    IF lv_val1 > 2147483647.
      lv_val1 = lv_val1 - 4294967296.
    ENDIF.
    lv_int = lv_val1.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).

  ENDMETHOD.

ENDCLASS.
