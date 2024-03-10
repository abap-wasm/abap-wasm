CLASS zcl_wasm_i64_store DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_align  TYPE int8
        iv_offset TYPE int8
      RAISING
        zcx_wasm.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_align TYPE int8.
    DATA mv_offset TYPE int8.
ENDCLASS.

CLASS zcl_wasm_i64_store IMPLEMENTATION.

  METHOD constructor.
    IF iv_align > zcl_wasm_memory=>c_alignment_64bit.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'alignment must not be larger than natural'.
    ENDIF.

    mv_align  = iv_align.
    mv_offset = iv_offset.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_i64_store(
      iv_align  = io_body->shift_u32( )
      iv_offset = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    CONSTANTS lc_length TYPE int8 VALUE 8.
    DATA lv_hex TYPE x LENGTH lc_length.

    DATA(li_linear) = io_memory->get_linear( ).

    DATA(lv_c) = io_memory->get_stack( )->pop( ).
    IF lv_c->get_type( ) <> zif_wasm_types=>c_value_type-i64.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64 store: expected i64'.
    ENDIF.

    DATA(lv_i) = io_memory->get_stack( )->pop_i32( )->get_signed( ).
    IF lv_i < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64 store: out of bounds'.
    ENDIF.

    lv_hex = CAST zcl_wasm_i64( lv_c )->get_signed( ).

* convert to little endian
    CONCATENATE lv_hex+7(1) lv_hex+6(1) lv_hex+5(1) lv_hex+4(1)
                lv_hex+3(1) lv_hex+2(1) lv_hex+1(1) lv_hex(1)
      INTO lv_hex IN BYTE MODE.

    li_linear->set(
      iv_offset = mv_offset + lv_i
      iv_bytes  = lv_hex ).
  ENDMETHOD.

ENDCLASS.
