CLASS zcl_wasm_i32_load8_u DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_align  TYPE int8
        iv_offset TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_align  TYPE int8.
    DATA mv_offset TYPE int8.
ENDCLASS.

CLASS zcl_wasm_i32_load8_u IMPLEMENTATION.

  METHOD constructor.
    mv_align  = iv_align.
    mv_offset = iv_offset.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_i32_load8_u(
      iv_align  = io_body->shift_u32( )
      iv_offset = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA lv_hex TYPE x LENGTH 1.
    DATA lv_int TYPE i.

    IF mv_align <> 0.
      RAISE EXCEPTION NEW zcx_wasm( text = |zcl_wasm_i32_load8_u, todo align <> 0| ).
    ENDIF.

    lv_hex = io_memory->linear_get(
      iv_length = 1
      iv_offset = mv_offset ).

    lv_int = lv_hex.
    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
