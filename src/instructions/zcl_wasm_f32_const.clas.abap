CLASS zcl_wasm_f32_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_float TYPE f.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_float TYPE f.
ENDCLASS.

CLASS zcl_wasm_f32_const IMPLEMENTATION.

  METHOD constructor.
* todo, ABAP float is double precision?
    mv_float = iv_float.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_f32_const( io_body->shift_f32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction ' ).
  ENDMETHOD.

ENDCLASS.
