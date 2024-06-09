CLASS zcl_wasm_elem_drop DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_elemidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_elemidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_elem_drop IMPLEMENTATION.

  METHOD constructor.
    mv_elemidx = iv_elemidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_elem_drop(
      iv_elemidx = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'todo, execute instruction zcl_wasm_elem_drop'.
  ENDMETHOD.

ENDCLASS.
