CLASS zcl_wasm_br_table DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_br_table IMPLEMENTATION.

  METHOD parse.
    ASSERT 1 = 'todo'.

    DATA(lv_length) = io_body->shift_u32( ).
    DO lv_length TIMES.
      io_body->shift_u32( ).
    ENDDO.
    io_body->shift_u32( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.
