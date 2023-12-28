CLASS zcl_wasm_br_if DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_labelidx TYPE i.

    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_labelidx TYPE i.
ENDCLASS.

CLASS zcl_wasm_br_if IMPLEMENTATION.

  METHOD constructor.
    mv_labelidx = iv_labelidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_br_if( io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.