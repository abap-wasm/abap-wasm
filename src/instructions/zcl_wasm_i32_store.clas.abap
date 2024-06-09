CLASS zcl_wasm_i32_store DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_align  TYPE int8
        iv_offset TYPE int8
      RAISING
        zcx_wasm.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    CONSTANTS gc_length TYPE int8 VALUE 4.
    DATA mv_align TYPE int8.
    DATA mv_offset TYPE int8.
ENDCLASS.

CLASS zcl_wasm_i32_store IMPLEMENTATION.

  METHOD constructor.
    "##feature-start=debug
    IF iv_align > zcl_wasm_memory=>c_alignment_32bit.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'alignment must not be larger than natural'.
    ENDIF.
    "##feature-end=debug

    mv_align  = iv_align.
    mv_offset = iv_offset.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_i32_store(
      iv_align  = io_body->shift_u32( )
      iv_offset = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    DATA lv_hex TYPE x LENGTH gc_length.

    lv_hex = io_memory->mi_stack->pop_i32( )->mv_value.
    DATA(lv_i) = io_memory->mi_stack->pop_i32( )->mv_value.
    "##feature-start=debug
    IF lv_i < 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i32 store: out of bounds'.
    ENDIF.
    "##feature-end=debug

* convert to little endian
    CONCATENATE lv_hex+3 lv_hex+2(1) lv_hex+1(1) lv_hex(1) INTO lv_hex IN BYTE MODE.

    io_memory->mi_linear->set(
      iv_offset = mv_offset + lv_i
      iv_bytes  = lv_hex ).

  ENDMETHOD.

ENDCLASS.
