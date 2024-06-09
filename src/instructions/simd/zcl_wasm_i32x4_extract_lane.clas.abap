CLASS zcl_wasm_i32x4_extract_lane DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32x4_extract_lane IMPLEMENTATION.

  METHOD parse.
    io_body->shift( 1 ).
    ri_instruction = NEW zcl_wasm_i32x4_extract_lane( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_i32x4_extract_lane todo|.
  ENDMETHOD.

ENDCLASS.
