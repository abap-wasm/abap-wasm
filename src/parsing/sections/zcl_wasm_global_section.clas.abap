CLASS zcl_wasm_global_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body         TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_global) TYPE REF TO zcl_wasm_global_section
      RAISING
        zcx_wasm.

    TYPES:
      BEGIN OF ty_global,
        type         TYPE x LENGTH 1,
        mut          TYPE x LENGTH 1,
        instructions TYPE zif_wasm_instruction=>ty_list,
      END OF ty_global.
    TYPES: ty_globals TYPE STANDARD TABLE OF ty_global WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        it_globals TYPE ty_globals OPTIONAL.

  PRIVATE SECTION.
    DATA mt_globals TYPE ty_globals.
ENDCLASS.

CLASS zcl_wasm_global_section IMPLEMENTATION.

  METHOD constructor.
    mt_globals = it_globals.
  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-globalsec

    DATA lt_globals TYPE ty_globals.
    DATA ls_global  LIKE LINE OF lt_globals.

    DO io_body->shift_u32( ) TIMES.
      CLEAR ls_global.
      ls_global-type = io_body->shift( 1 ).
      ls_global-mut = io_body->shift( 1 ).

      zcl_wasm_instructions=>parse(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = DATA(lv_last_opcode)
          et_instructions = ls_global-instructions ).
      IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
        RAISE EXCEPTION NEW zcx_wasm( text = |parse_global, expected end| ).
      ENDIF.
      INSERT ls_global INTO TABLE lt_globals.
    ENDDO.

    ro_global = NEW zcl_wasm_global_section( lt_globals ).

  ENDMETHOD.

ENDCLASS.
