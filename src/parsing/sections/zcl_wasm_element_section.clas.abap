CLASS zcl_wasm_element_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_section) TYPE REF TO zcl_wasm_element_section
      RAISING
        zcx_wasm.

    METHODS instantiate
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_element,
             type     TYPE i,
             expr     TYPE STANDARD TABLE OF REF TO zif_wasm_instruction WITH DEFAULT KEY,
             elemkind TYPE zif_wasm_types=>ty_type,
             funcidx  TYPE STANDARD TABLE OF int8 WITH DEFAULT KEY,
             tableidx TYPE i,
             init     TYPE STANDARD TABLE OF zif_wasm_instruction=>ty_list WITH DEFAULT KEY,
           END OF ty_element.
    DATA mt_elements TYPE STANDARD TABLE OF ty_element WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_wasm_element_section IMPLEMENTATION.

  METHOD instantiate.

* type = 0 + 2 + 4 + 6 is active

    DATA li_value TYPE REF TO zif_wasm_value.
    DATA ls_control TYPE zif_wasm_instruction=>ty_control.

    TRY.
        LOOP AT mt_elements INTO DATA(ls_element).
          " WRITE / |element section type: { ls_element-type }|.
          CASE ls_element-type.
            WHEN 0.
              LOOP AT ls_element-expr INTO DATA(lo_instruction).
                lo_instruction->execute(
                  EXPORTING
                    io_memory  = io_memory
                    io_module  = NEW zcl_wasm_module( )
                  CHANGING
                    cs_control = ls_control ).
              ENDLOOP.
              DATA(lv_offset) = io_memory->mi_stack->pop_i32( )->mv_value.
              " WRITE / |offset: { lv_offset }|.
              " WRITE / |length: { lines( ls_element-funcidx ) }|.

              LOOP AT ls_element-funcidx INTO DATA(lv_funcidx).
                DATA(lo_ref) = NEW zcl_wasm_funcref( lv_funcidx ).
                io_memory->table_set(
                  iv_tableidx = ls_element-tableidx
                  iv_offset   = lv_offset
                  ii_value    = lo_ref ).
                lv_offset = lv_offset + 1.
              ENDLOOP.
            WHEN 2.
              LOOP AT ls_element-expr INTO lo_instruction.
                lo_instruction->execute(
                  EXPORTING
                    io_memory  = io_memory
                    io_module  = NEW zcl_wasm_module( )
                  CHANGING
                    cs_control = ls_control ).
              ENDLOOP.
              lv_offset = io_memory->mi_stack->pop_i32( )->mv_value.

              LOOP AT ls_element-funcidx INTO lv_funcidx.
* todo, is the type derived from the table? elemkind seems wrong?
                " CASE ls_element-elemkind.
                "   WHEN zif_wasm_types=>c_reftype-funcref.
                li_value ?= NEW zcl_wasm_funcref( lv_funcidx ).
                "   WHEN OTHERS.
                "     RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate 2: todo type { ls_element-elemkind }|.
                " ENDCASE.
                io_memory->table_set(
                  iv_tableidx = ls_element-tableidx
                  iv_offset   = lv_offset
                  ii_value    = li_value ).
                lv_offset = lv_offset + 1.
              ENDLOOP.
            WHEN 4.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate 4: todo|.
            WHEN 6.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate 6: todo|.
          ENDCASE.
        ENDLOOP.
      CATCH cx_static_check INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_wasm
          EXPORTING
            text = |instantiate element section, failed to execute instructions: { lx_error->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-elemsec

    DATA ls_element      LIKE LINE OF mt_elements.
    DATA lt_instructions TYPE zif_wasm_instruction=>ty_list.

    ro_section = NEW #( ).

    DATA(lv_times) = io_body->shift_u32( ).
    DO lv_times TIMES.
      CLEAR ls_element.
      ls_element-type = io_body->shift_u32( ).

      CASE ls_element-type.
        WHEN 0.
          ls_element-elemkind = zif_wasm_types=>c_reftype-funcref.
          ls_element-tableidx = 0.

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = DATA(lv_last_opcode)
              et_instructions = ls_element-expr ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
          ENDIF.

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            INSERT io_body->shift_u32( ) INTO TABLE ls_element-funcidx.
          ENDDO.
        WHEN 1.
          ls_element-elemkind = io_body->shift( 1 ).

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            INSERT io_body->shift_u32( ) INTO TABLE ls_element-funcidx.
          ENDDO.
        WHEN 2.
          ls_element-tableidx = io_body->shift_u32( ).

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = ls_element-expr ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
          ENDIF.

          ls_element-elemkind = io_body->shift( 1 ).

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            INSERT io_body->shift_u32( ) INTO TABLE ls_element-funcidx.
          ENDDO.
        WHEN 3.
          ls_element-elemkind = io_body->shift( 1 ).

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            INSERT io_body->shift_u32( ) INTO TABLE ls_element-funcidx.
          ENDDO.
        WHEN 4.
          ls_element-elemkind = zif_wasm_types=>c_reftype-funcref.
          ls_element-tableidx = 0.

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = ls_element-expr ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
          ENDIF.

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            INSERT lt_instructions INTO TABLE ls_element-init.
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
            ENDIF.
          ENDDO.
        WHEN 5.
          ls_element-elemkind = io_body->shift( 1 ).

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            INSERT lt_instructions INTO TABLE ls_element-init.
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
            ENDIF.
          ENDDO.
        WHEN 6.
          ls_element-tableidx = io_body->shift_u32( ).

          zcl_wasm_instructions=>parse(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = ls_element-expr ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
          ENDIF.

          ls_element-elemkind = io_body->shift( 1 ).

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            INSERT lt_instructions INTO TABLE ls_element-init.
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
            ENDIF.
          ENDDO.
        WHEN 7.
          ls_element-elemkind = io_body->shift( 1 ).

          lv_times = io_body->shift_u32( ).
          DO lv_times TIMES.
            zcl_wasm_instructions=>parse(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            INSERT lt_instructions INTO TABLE ls_element-init.
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_element: expected end|.
            ENDIF.
          ENDDO.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |elementtype: { ls_element-type }|.
      ENDCASE.

      INSERT ls_element INTO TABLE ro_section->mt_elements.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
