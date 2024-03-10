CLASS zcl_wasm_table_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_table,
             reftype TYPE zif_wasm_types=>ty_type,
             limit   TYPE zif_wasm_types=>ty_limit,
           END OF ty_table.

    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_section) TYPE REF TO zcl_wasm_table_section
      RAISING
        zcx_wasm.

    METHODS instantiate
      IMPORTING io_memory TYPE REF TO zcl_wasm_memory
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mt_tables TYPE STANDARD TABLE OF ty_table WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_wasm_table_section IMPLEMENTATION.

  METHOD instantiate.
    LOOP AT mt_tables INTO DATA(ls_table).
      io_memory->table_add( ls_table ).
    ENDLOOP.
  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-tablesec

    DATA ls_table LIKE LINE OF mt_tables.

    ro_section = NEW zcl_wasm_table_section( ).

    DO io_body->shift_u32( ) TIMES.
      CLEAR ls_table.
      ls_table-reftype = io_body->shift( 1 ).

      CASE ls_table-reftype.
        WHEN zif_wasm_types=>c_reftype-funcref OR zif_wasm_types=>c_reftype-externref.
          CASE io_body->shift( 1 ).
            WHEN zif_wasm_types=>c_limit-min.
              ls_table-limit-min = io_body->shift_u32( ).
              ls_table-limit-max = 0.
            WHEN zif_wasm_types=>c_limit-max.
              ls_table-limit-min = io_body->shift_u32( ).
              ls_table-limit-max = io_body->shift_u32( ).
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_table: todo|.
          ENDCASE.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_table: todo|.
      ENDCASE.
      INSERT ls_table INTO TABLE ro_section->mt_tables.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
