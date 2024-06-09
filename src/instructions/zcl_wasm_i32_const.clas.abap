CLASS zcl_wasm_i32_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_value TYPE i.

    CLASS-METHODS parse
      IMPORTING io_body               TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
  PRIVATE SECTION.
    DATA mi_value TYPE REF TO zcl_wasm_i32.
ENDCLASS.

CLASS zcl_wasm_i32_const IMPLEMENTATION.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_i32_const( io_body->shift_i32( ) ).
  ENDMETHOD.

  METHOD constructor.
* the value objects are immutable, so we can reuse them
    CASE iv_value.
      WHEN 0.
        mi_value = zcl_wasm_i32=>gc_zero.
      WHEN 1.
        mi_value = zcl_wasm_i32=>gc_one.
      WHEN OTHERS.
        mi_value = zcl_wasm_i32=>from_signed( iv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-instr-numeric-mathsf-const-c
    io_memory->mi_stack->push( mi_value ).
  ENDMETHOD.

ENDCLASS.
