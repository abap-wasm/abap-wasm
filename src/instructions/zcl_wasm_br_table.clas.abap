CLASS zcl_wasm_br_table DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    TYPES ty_branches TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        it_branches TYPE ty_branches
        iv_default  TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mt_branches TYPE ty_branches.
    DATA mv_default  TYPE int8.
ENDCLASS.

CLASS zcl_wasm_br_table IMPLEMENTATION.

  METHOD constructor.
    mt_branches = it_branches.
    mv_default  = iv_default.
  ENDMETHOD.

  METHOD parse.

    DATA lt_branches TYPE ty_branches.

    DATA(lv_length) = io_body->shift_u32( ).
    DO lv_length TIMES.
      APPEND io_body->shift_u32( ) TO lt_branches.
    ENDDO.

    DATA(lv_default) = io_body->shift_u32( ).

    ri_instruction = NEW zcl_wasm_br_table(
      it_branches = lt_branches
      iv_default  = lv_default ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    ASSERT 1 = 'todo'.
  ENDMETHOD.

ENDCLASS.
