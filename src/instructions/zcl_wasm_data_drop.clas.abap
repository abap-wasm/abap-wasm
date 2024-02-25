CLASS zcl_wasm_data_drop DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_dataidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
    DATA mv_dataidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_data_drop IMPLEMENTATION.

  METHOD constructor.
    mv_dataidx = iv_dataidx.
  ENDMETHOD.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_data_drop( io_body->shift_u32( ) ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_data_drop' ).
  ENDMETHOD.

ENDCLASS.
