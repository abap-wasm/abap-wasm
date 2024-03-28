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
        io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    METHODS get_passive
      IMPORTING
        iv_dataidx      TYPE int8
      RETURNING
        VALUE(rv_bytes) TYPE xstring
      RAISING
        zcx_wasm.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_active,
              memidx       TYPE i,
              instructions TYPE zif_wasm_instruction=>ty_list,
              bytes        TYPE xstring,
           END OF ty_active.
    DATA mt_active TYPE STANDARD TABLE OF ty_active WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_passive,
              dataidx TYPE int8,
              bytes   TYPE xstring,
           END OF ty_passive.
    DATA mt_passive TYPE STANDARD TABLE OF ty_passive WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_wasm_data_section IMPLEMENTATION.

  METHOD get_passive.

    READ TABLE mt_passive ASSIGNING FIELD-SYMBOL(<ls_passive>) WITH KEY dataidx = iv_dataidx.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |get_passive, dataidx: { iv_dataidx } not found|.
    ENDIF.

    rv_bytes = <ls_passive>-bytes.

  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-datasec

    DATA lt_instructions TYPE zif_wasm_instruction=>ty_list.

    ro_data = NEW zcl_wasm_data_section( ).

    DATA(lv_times) = io_body->shift_u32( ).
    DO lv_times TIMES.
      DATA(lv_index) = sy-index - 1.
      DATA(lv_type) = io_body->shift_u32( ).
      CLEAR lt_instructions.

      CASE lv_type.
        WHEN 0.
* active, memidx = 0
          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = DATA(lv_last_opcode)
              et_instructions = lt_instructions ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          DATA(lv_bytes) = io_body->shift( io_body->shift_u32( ) ).

          APPEND VALUE #(
            memidx       = 0
            instructions = lt_instructions
            bytes        = lv_bytes ) TO ro_data->mt_active.
        WHEN 1.
* passive
          lv_bytes = io_body->shift( io_body->shift_u32( ) ).
          APPEND VALUE #(
            dataidx = lv_index
            bytes   = lv_bytes ) TO ro_data->mt_passive.
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
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_data, type: { lv_type }|.
      ENDCASE.

    ENDDO.

  ENDMETHOD.

  METHOD instantiate.

* https://webassembly.github.io/spec/core/syntax/modules.html#syntax-data
* An active data segment copies its contents into a memory during instantiation
* In the current version of WebAssembly, at most one memory is allowed in a module.

    DATA ls_control TYPE zif_wasm_instruction=>ty_control.

    LOOP AT mt_active INTO DATA(ls_active).
      TRY.
          LOOP AT ls_active-instructions INTO DATA(lo_instruction).
            lo_instruction->execute(
              EXPORTING
                io_memory  = io_memory
                io_module  = NEW zcl_wasm_module( )
              CHANGING
                cs_control = ls_control ).
          ENDLOOP.
        CATCH cx_static_check INTO DATA(lx_error).
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate data, failed to execute instructions: { lx_error->get_text( ) }|.
      ENDTRY.

      DATA(lv_offset) = io_memory->mi_stack->pop_i32( )->get_unsigned( ).

      IF io_memory->mi_linear IS INITIAL.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate data, no linear memory|.
      ENDIF.

      " WRITE / |active data, offset { lv_offset }|.
      io_memory->mi_linear->set(
        iv_offset = lv_offset
        iv_bytes  = ls_active-bytes ).
    ENDLOOP.
    " WRITE / |instantiate data section, done|.

  ENDMETHOD.

ENDCLASS.
