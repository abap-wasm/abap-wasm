CLASS zcl_wasm_ref_null DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_ref_type TYPE xstring.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_ref_type TYPE xstring.
ENDCLASS.

CLASS zcl_wasm_ref_null IMPLEMENTATION.

  METHOD constructor.
    mv_ref_type = iv_ref_type.
  ENDMETHOD.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_ref_null( io_body->shift( 1 ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_ref_null' ).
  ENDMETHOD.

ENDCLASS.
