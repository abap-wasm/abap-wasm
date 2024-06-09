CLASS zcl_wasm_v128_load DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING
        !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_v128_load IMPLEMENTATION.

  METHOD parse.
    io_body->shift_u32( ).
    io_body->shift_u32( ).
    ri_instruction = NEW zcl_wasm_v128_load( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_v128_load todo|.
  ENDMETHOD.

ENDCLASS.
