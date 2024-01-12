CLASS zcl_wasm_select_star DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_select_star IMPLEMENTATION.

  METHOD parse.
    DO io_body->shift_u32( ) TIMES.
      io_body->shift( 1 ).
    ENDDO.
* todo, pass types to constructor
    ri_instruction = NEW zcl_wasm_select_star( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.