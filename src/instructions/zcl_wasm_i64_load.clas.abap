CLASS zcl_wasm_i64_load DEFINITION PUBLIC.
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
      RAISING   zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS gc_length TYPE int8 VALUE 8.
    CONSTANTS gc_ff     TYPE x LENGTH 16 VALUE 'FFFFFFFFFFFFFFFF'.

    DATA mv_align  TYPE int8.
    DATA mv_offset TYPE int8.
ENDCLASS.



CLASS zcl_wasm_i64_load IMPLEMENTATION.


  METHOD constructor.
    "##feature-start=debug
    IF iv_align > zcl_wasm_memory=>c_alignment_64bit.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'alignment must not be larger than natural'.
    ENDIF.
    "##feature-end=debug

    mv_align  = iv_align.
    mv_offset = iv_offset.
  ENDMETHOD.


  METHOD parse.
    ri_instruction = NEW zcl_wasm_i64_load(
      iv_align  = io_body->shift_u32( )
      iv_offset = io_body->shift_u32( ) ).
  ENDMETHOD.


  METHOD zif_wasm_instruction~execute.
    DATA lv_hex    TYPE x LENGTH gc_length.
    DATA lv_int    TYPE i.
    DATA lv_int8   TYPE int8.
    DATA lv_factor TYPE int8.

    DATA(lv_i) = io_memory->mi_stack->pop_i32( )->mv_value.
    "##feature-start=debug
    IF lv_i < 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_i64_load: out of bounds { lv_i }|.
    ENDIF.
    "##feature-end=debug

    lv_hex = io_memory->mi_linear->get(
      iv_length = gc_length
      iv_align  = mv_align
      iv_offset = mv_offset + lv_i ).

* todo: all of this code can be reduced to single statement? just take care of the endianess? lv_int8 = lv_hex8.
* and make sure the transpiler works
    IF lv_hex(5) = '0000000000'.
      lv_int = lv_hex+4.
      lv_int8 = lv_int.
    ELSE.
      GET BIT 1 OF lv_hex INTO DATA(lv_sign).
      " WRITE: / 'sign', lv_sign.

      IF lv_sign = 1.
        lv_hex = lv_hex BIT-XOR gc_ff.
      ENDIF.
      " WRITE / lv_hex.

      lv_factor = 1.
      DO 63 TIMES.
        DATA(lv_offset) = 65 - sy-index.
        GET BIT lv_offset OF lv_hex INTO DATA(lv_bit).
        IF lv_bit = 1.
          lv_int8 = lv_int8 + lv_factor.
        ENDIF.
        IF lv_offset <> 2.
          lv_factor = lv_factor * 2.
        ENDIF.
      ENDDO.

      IF lv_sign = 1.
        lv_int8 = lv_int8 * -1.
        lv_int8 = lv_int8 - 1.
      ENDIF.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_int8 ) ).
  ENDMETHOD.
ENDCLASS.
