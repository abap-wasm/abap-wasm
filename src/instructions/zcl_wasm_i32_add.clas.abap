CLASS zcl_wasm_i32_add DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_add IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_add( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop

    DATA lv_val1 TYPE int8.
    DATA lv_val2 TYPE int8.

    lv_val1 = io_memory->mi_stack->pop_i32( )->get_signed( ).
    lv_val2 = io_memory->mi_stack->pop_i32( )->get_signed( ).

    lv_val1 = lv_val1 + lv_val2.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_int8( lv_val1 ) ).

  ENDMETHOD.

ENDCLASS.
