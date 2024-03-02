CLASS zcl_wasm_f64_store DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_align  TYPE int8
        iv_offset TYPE int8
      RAISING
        zcx_wasm.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_align TYPE int8.
    DATA mv_offset TYPE int8.
ENDCLASS.

CLASS zcl_wasm_f64_store IMPLEMENTATION.

  METHOD constructor.
    IF iv_align > zcl_wasm_memory=>c_alignment_64bit.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'alignment must not be larger than natural'.
    ENDIF.

    mv_align  = iv_align.
    mv_offset = iv_offset.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_f64_store(
      iv_align  = io_body->shift_u32( )
      iv_offset = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'todo, execute instruction zcl_wasm_f64_store'.
  ENDMETHOD.

ENDCLASS.
