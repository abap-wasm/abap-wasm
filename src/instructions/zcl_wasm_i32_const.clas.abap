CLASS zcl_wasm_i32_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_value TYPE i.

    CLASS-METHODS parse
      IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_value TYPE i.
ENDCLASS.

CLASS zcl_wasm_i32_const IMPLEMENTATION.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_i32_const( io_body->shift_i32( ) ).
  ENDMETHOD.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-instr-numeric-mathsf-const-c
    io_memory->stack_push( zcl_wasm_i32=>from_signed( mv_value ) ).
  ENDMETHOD.

ENDCLASS.
