CLASS zcl_wasm_call_indirect DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_typeidx  TYPE i
        iv_tableidx TYPE i.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_typeidx  TYPE i.
    DATA mv_tableidx TYPE i.
ENDCLASS.

CLASS zcl_wasm_call_indirect IMPLEMENTATION.

  METHOD constructor.
    mv_typeidx  = iv_typeidx.
    mv_tableidx = iv_tableidx.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

  METHOD parse.

    DATA(lv_typeidx) = io_body->shift_u32( ).
    DATA(lv_tableidx) = io_body->shift_u32( ).

    ri_instruction = NEW zcl_wasm_call_indirect(
      iv_typeidx  = lv_typeidx
      iv_tableidx = lv_tableidx ).

  ENDMETHOD.

ENDCLASS.
