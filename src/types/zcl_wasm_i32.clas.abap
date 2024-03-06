CLASS zcl_wasm_i32 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

    CLASS-METHODS from_signed
      IMPORTING
        !iv_value TYPE i
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i32.
    CLASS-METHODS from_unsigned
      IMPORTING
        !iv_value TYPE int8
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i32.
    CLASS-METHODS from_int8
      IMPORTING
        !iv_value TYPE int8
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i32.

    METHODS get_signed
      RETURNING
        VALUE(rv_value) TYPE i .
    METHODS get_unsigned
      RETURNING
        VALUE(rv_value) TYPE int8 .

    CLASS-METHODS le_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS gt_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS ge_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS ge_u
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS gt_u
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS le_u
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS eqz
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS eq
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS and
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS or
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS xor
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS ne
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

* convert a potentially overflowing arithmethic operation back to int4
    CLASS-METHODS int8_to_int4
      IMPORTING
        !iv_value       TYPE int8
      RETURNING
        VALUE(rv_value) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
* https://webassembly.github.io/spec/core/syntax/types.html
* "Integers are not inherently signed or unsigned, their interpretation is determined by individual operations."

* the internal representation is signed in abap-wasm,
    DATA mv_value TYPE i .
ENDCLASS.



CLASS zcl_wasm_i32 IMPLEMENTATION.

  METHOD zif_wasm_value~human_readable_value.
    rv_string = |i32: { mv_value }|.
  ENDMETHOD.

  METHOD from_int8.
    ro_value = NEW #( ).
    ro_value->mv_value = int8_to_int4( iv_value ).
  ENDMETHOD.

  METHOD int8_to_int4.
    DATA lv_res TYPE int8.

    lv_res = iv_value MOD 4294967296.
    IF lv_res > 2147483647.
      lv_res = lv_res - 4294967296.
    ENDIF.
    rv_value = lv_res.
  ENDMETHOD.

  METHOD from_signed.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_value.
  ENDMETHOD.

  METHOD from_unsigned.
    ro_value = NEW #( ).
* todo: throw error if input is too large?
    IF iv_value > cl_abap_math=>max_int4.
      ro_value->mv_value = iv_value - cl_abap_math=>max_int4 - cl_abap_math=>max_int4 - 2.
    ELSE.
      ro_value->mv_value = iv_value.
    ENDIF.
  ENDMETHOD.

  METHOD get_unsigned.
    rv_value = mv_value.
    IF rv_value < 0.
      rv_value = rv_value + cl_abap_math=>max_int4 + cl_abap_math=>max_int4 + 2.
    ENDIF.
  ENDMETHOD.

  METHOD get_signed.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD le_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'le_s, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) >= lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD gt_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'gt_s, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) < lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD ge_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'ge_s, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) <= lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD ge_u.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'ge_u, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_unsigned( ) <= lo_val2->get_unsigned( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD gt_u.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'gt_u, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_unsigned( ) < lo_val2->get_unsigned( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD le_u.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt_u, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_unsigned( ) >= lo_val2->get_unsigned( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD eqz.

    ASSERT io_memory->stack_length( ) >= 1.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      io_memory->stack_push( from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD eq.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 = lv_val2.
      io_memory->stack_push( from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD ne.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 <> lv_val2.
      io_memory->stack_push( from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_types=>c_value_type-i32.

  ENDMETHOD.

  METHOD and.

    DATA lv_hex1 TYPE x LENGTH 4.
    DATA lv_hex2 TYPE x LENGTH 4.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    lv_hex1 = lv_val1.
    lv_hex2 = lv_val2.

    lv_hex1 = lv_hex1 BIT-AND lv_hex2.
    lv_val1 = lv_hex1.

    io_memory->stack_push( from_signed( lv_val1 ) ).

  ENDMETHOD.

  METHOD or.

    DATA lv_hex1 TYPE x LENGTH 4.
    DATA lv_hex2 TYPE x LENGTH 4.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    lv_hex1 = lv_val1.
    lv_hex2 = lv_val2.

    lv_hex1 = lv_hex1 BIT-OR lv_hex2.
    lv_val1 = lv_hex1.

    io_memory->stack_push( from_signed( lv_val1 ) ).

  ENDMETHOD.

  METHOD xor.

    DATA lv_hex1 TYPE x LENGTH 4.
    DATA lv_hex2 TYPE x LENGTH 4.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    lv_hex1 = lv_val1.
    lv_hex2 = lv_val2.

    lv_hex1 = lv_hex1 BIT-XOR lv_hex2.
    lv_val1 = lv_hex1.

    io_memory->stack_push( from_signed( lv_val1 ) ).
  ENDMETHOD.

ENDCLASS.
