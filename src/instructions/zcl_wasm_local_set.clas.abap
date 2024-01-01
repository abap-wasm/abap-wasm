CLASS zcl_wasm_local_set DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_localidx TYPE i.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_localidx TYPE i.
ENDCLASS.

CLASS zcl_wasm_local_set IMPLEMENTATION.

  METHOD constructor.
    mv_localidx = iv_localidx.
  ENDMETHOD.

  METHOD parse.
* todo, singletons?
    ri_instruction = NEW zcl_wasm_local_set( io_body->shift_int( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.
