CLASS zcl_wasm_data_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body       TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_data) TYPE REF TO zcl_wasm_data_section
      RAISING
        zcx_wasm.

    METHODS instantiate
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_active,
              memidx       TYPE i,
              instructions TYPE zif_wasm_instruction=>ty_list,
              bytes        TYPE xstring,
           END OF ty_active.
    DATA mt_active TYPE STANDARD TABLE OF ty_active WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_wasm_data_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-datasec

    ro_data = NEW zcl_wasm_data_section( ).

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_type) = io_body->shift_u32( ).

      CASE lv_type.
        WHEN 0.
* active, memidx = 0
          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = DATA(lv_last_opcode)
              et_instructions = DATA(lt_instructions) ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          DATA(lv_bytes) = io_body->shift( io_body->shift_u32( ) ).

          APPEND VALUE #(
            memidx       = 0
            instructions = lt_instructions
            bytes        = lv_bytes ) TO ro_data->mt_active.
        WHEN 1.
* passive
          lv_bytes = io_body->shift( io_body->shift_u32( ) ).
* todo
        WHEN 2.
* active, memidx = dynamic
          DATA(lv_memidx) = io_body->shift_u32( ).

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          lv_bytes = io_body->shift( io_body->shift_u32( ) ).

          APPEND VALUE #(
            memidx       = lv_memidx
            instructions = lt_instructions
            bytes        = lv_bytes ) TO ro_data->mt_active.
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |parse_data, type: { lv_type }| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

  METHOD instantiate.

* https://webassembly.github.io/spec/core/syntax/modules.html#syntax-data
* An active data segment copies its contents into a memory during instantiation
* In the current version of WebAssembly, at most one memory is allowed in a module.

    LOOP AT mt_active INTO DATA(ls_active).
      io_memory->linear_set(
        iv_offset = 0 " todo
        iv_bytes  = ls_active-bytes ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
