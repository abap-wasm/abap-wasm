CLASS zcl_wasm_f64_store DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_align  TYPE i
        iv_offset TYPE i.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_align TYPE i.
    DATA mv_offset TYPE i.
ENDCLASS.

CLASS zcl_wasm_f64_store IMPLEMENTATION.

  METHOD constructor.
    mv_align  = iv_align.
    mv_offset = iv_offset.
  ENDMETHOD.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_f64_store(
      iv_align  = io_body->shift_u32( )
      iv_offset = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.
