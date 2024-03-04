CLASS zcl_wasm_i64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

* note: unsigned i64 are larger than int8
    CLASS-METHODS from_unsigned
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i64
      RAISING
        zcx_wasm.

    CLASS-METHODS from_signed
      IMPORTING
        !iv_value       TYPE int8
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i64
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

    CLASS-METHODS lt_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS le_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS mul
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS add
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

* only used in testclasses?
    METHODS get_unsigned
      RETURNING
        VALUE(rv_value) TYPE string
      RAISING
        zcx_wasm.

    METHODS get_signed
      RETURNING
        VALUE(rv_value) TYPE int8
      RAISING
        zcx_wasm.

    CLASS-METHODS eqz
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS ne
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS eq
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS sub
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS div_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
* todo, use packed? nah int8 is long enough, but need to handle unsigned
    DATA mv_value TYPE int8 .
ENDCLASS.

CLASS zcl_wasm_i64 IMPLEMENTATION.

  METHOD mul.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_signed( lo_val1->get_signed( ) * lo_val2->get_signed( ) ) ).

  ENDMETHOD.

  METHOD le_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'le_s, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) >= lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD div_s.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64.div_s, division by zero'.
    ENDIF.

* division is truncating, so round towards zero
    IF sign( lv_val1 ) <> sign( lv_val2 ).
      io_memory->stack_push( from_signed( -1 * ( abs( lv_val2 ) DIV abs( lv_val1 ) ) ) ).
    ELSE.
      io_memory->stack_push( from_signed( lv_val2 DIV lv_val1 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD lt_s.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-relop-mathit-relop

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt_s, expected two variables on stack'.
    ENDIF.

    TRY.
        DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
        DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt_s, wrong types on stack'.
    ENDTRY.

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) > lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD sub.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_signed( lo_val2->get_signed( ) - lo_val1->get_signed( ) ) ).

  ENDMETHOD.

  METHOD get_signed.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD zif_wasm_value~human_readable_value.
    rv_string = |i64: { mv_value }|.
  ENDMETHOD.

  METHOD add.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_signed( lo_val1->get_signed( ) + lo_val2->get_signed( ) ) ).

  ENDMETHOD.

  METHOD gt_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'gt_s, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) < lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD ge_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'ge_s, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) <= lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD from_unsigned.
    IF iv_value CN '-0123456789'.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64, from_unsigned, unexpected value'.
    ENDIF.

    CASE iv_value.
      WHEN '18446744073709551615'.
        ro_value = NEW #( ).
        ro_value->mv_value = -1.
        RETURN.
      WHEN '18446744073709551614'.
        ro_value = NEW #( ).
        ro_value->mv_value = -2.
        RETURN.
      WHEN '18446744073709551613'.
        ro_value = NEW #( ).
        ro_value->mv_value = -3.
        RETURN.
      WHEN '18446744073709551612'.
        ro_value = NEW #( ).
        ro_value->mv_value = -4.
        RETURN.
      WHEN '18446744073709551611'.
        ro_value = NEW #( ).
        ro_value->mv_value = -5.
        RETURN.
      WHEN '18446744073709551609'.
        ro_value = NEW #( ).
        ro_value->mv_value = -7.
        RETURN.
      WHEN '18446744073709551601'.
        ro_value = NEW #( ).
        ro_value->mv_value = -15.
        RETURN.
      WHEN '18446744073709451616'.
        ro_value = NEW #( ).
        ro_value->mv_value = -100000.
        RETURN.
      WHEN '12297829381041378645'.
        ro_value = NEW #( ).
        ro_value->mv_value = -6148914692668172971.
        RETURN.
      WHEN '4611686018427387904'.
        ro_value = NEW #( ).
        ro_value->mv_value = 4611686018427387904.
        RETURN.
      WHEN '1152921504606846976'.
        ro_value = NEW #( ).
        ro_value->mv_value = 1152921504606846976.
        RETURN.
      WHEN '9223372036854775807'.
        ro_value = NEW #( ).
        ro_value->mv_value = 9223372036854775807.
        RETURN.
      WHEN '9223372036854775808'.
        ro_value = NEW #( ).
        ro_value->mv_value = -9223372036854775808.
        RETURN.
      WHEN '1311768467463733248'.
        ro_value = NEW #( ).
        ro_value->mv_value = 1311768467463733248.
        RETURN.
    ENDCASE.

    IF strlen( iv_value ) > 18.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |i64, from_unsigned, value too long, todo, "{ iv_value }"|.
    ENDIF.

    ro_value = NEW #( ).
    ro_value->mv_value = iv_value.
  ENDMETHOD.

  METHOD from_signed.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_value.
  ENDMETHOD.

  METHOD get_unsigned.
    IF mv_value < 0.
      CASE mv_value.
        WHEN -1.
          rv_value = '18446744073709551615'.
        WHEN -2.
          rv_value = '18446744073709551614'.
        WHEN -3.
          rv_value = '18446744073709551613'.
        WHEN -4.
          rv_value = '18446744073709551612'.
        WHEN -5.
          rv_value = '18446744073709551611'.
        WHEN -15.
          rv_value = '18446744073709551601'.
        WHEN -128.
          rv_value = '18446744073709551488'.
        WHEN -32768.
          rv_value = '18446744073709518848'.
        WHEN -9223372036854775808.
          rv_value = '9223372036854775808'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |i64, todo get_unsigned, value is negative: { mv_value }|.
      ENDCASE.
      RETURN.
    ENDIF.

    rv_value = |{ mv_value }|.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.
    rv_type = zcl_wasm_types=>c_value_type-i64.
  ENDMETHOD.

  METHOD eqz.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64, eqz, expected value on stack'.
    ENDIF.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->mv_value.

    IF lv_val1 = 0.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD ne.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 <> lv_val2.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD eq.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 = lv_val2.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
