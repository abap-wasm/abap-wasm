CLASS zcl_wasm_i32_and DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_and IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_i32_and( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>and( io_memory ).
  ENDMETHOD.

ENDCLASS.
