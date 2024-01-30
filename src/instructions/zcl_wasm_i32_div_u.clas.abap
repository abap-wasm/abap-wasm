CLASS zcl_wasm_i32_div_u DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_div_u IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_i32_div_u( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>div_u( io_memory ).
  ENDMETHOD.

ENDCLASS.
