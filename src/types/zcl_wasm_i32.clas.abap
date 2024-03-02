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

    METHODS get_signed
      RETURNING
        VALUE(rv_value) TYPE i .
    METHODS get_unsigned
      RETURNING
        VALUE(rv_value) TYPE int8 .

    CLASS-METHODS add
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS shl
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS mul
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS div_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS rem_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS rem_u
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS div_u
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS lt_s
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
    CLASS-METHODS lt_u
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
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
    CLASS-METHODS sub
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

  METHOD add.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop

    ASSERT io_memory->stack_length( ) >= 2.

    TRY.
        DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
        DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION NEW zcx_wasm( text = 'i32 add, wrong types on stack' ).
    ENDTRY.

    io_memory->stack_push( from_signed( lo_val1->get_signed( ) + lo_val2->get_signed( ) ) ).

  ENDMETHOD.

  METHOD mul.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_signed( lo_val1->get_signed( ) * lo_val2->get_signed( ) ) ).

  ENDMETHOD.

  METHOD shl.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-ishl-mathrm-ishl-n-i-1-i-2

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ) MOD 32.
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    DO lv_val1 TIMES.
      lv_val2 = lv_val2 * 2.
    ENDDO.

    io_memory->stack_push( from_signed( lv_val2 ) ).

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


  METHOD lt_s.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-relop-mathit-relop

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION NEW zcx_wasm( text = 'lt_s, expected two variables on stack' ).
    ENDIF.

    TRY.
        DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
        DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION NEW zcx_wasm( text = 'lt_s, wrong types on stack' ).
    ENDTRY.

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) > lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD le_s.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION NEW zcx_wasm( text = 'le_s, expected two variables on stack' ).
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
      RAISE EXCEPTION NEW zcx_wasm( text = 'le_s, expected two variables on stack' ).
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
      RAISE EXCEPTION NEW zcx_wasm( text = 'ge_s, expected two variables on stack' ).
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
      RAISE EXCEPTION NEW zcx_wasm( text = 'ge_u, expected two variables on stack' ).
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
      RAISE EXCEPTION NEW zcx_wasm( text = 'gt_u, expected two variables on stack' ).
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
      RAISE EXCEPTION NEW zcx_wasm( text = 'lt_u, expected two variables on stack' ).
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_unsigned( ) >= lo_val2->get_unsigned( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD lt_u.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION NEW zcx_wasm( text = 'lt_u, expected two variables on stack' ).
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_unsigned( ) > lo_val2->get_unsigned( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.


  METHOD sub.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-binop-mathit-binop

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_signed( lo_val2->get_signed( ) - lo_val1->get_signed( ) ) ).

  ENDMETHOD.

  METHOD div_s.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i32.div_s, division by zero' ).
    ENDIF.

* division is truncating, so round towards zero
    IF sign( lv_val1 ) <> sign( lv_val2 ).
      io_memory->stack_push( from_signed( -1 * ( abs( lv_val2 ) DIV abs( lv_val1 ) ) ) ).
    ELSE.
      io_memory->stack_push( from_signed( lv_val2 DIV lv_val1 ) ).
    ENDIF.

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

  METHOD rem_s.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i32.rem_s, division by zero' ).
    ENDIF.

    DATA(lv_result) = abs( lv_val2 ) MOD abs( lv_val1 ).
    IF lv_val2 < 0.
      lv_result = lv_result * -1.
    ENDIF.
    io_memory->stack_push( from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD rem_u.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_unsigned( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_unsigned( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i32.rem_u, division by zero' ).
    ENDIF.

    IF lv_val1 < 0.
      lv_val1 = lv_val1 * -1.
    ENDIF.
    IF lv_val2 < 0.
      lv_val2 = lv_val2 * -1.
    ENDIF.

    DATA(lv_result) = lv_val2 MOD lv_val1.
    IF lv_val1 < 0.
      lv_result = lv_result * -1.
    ENDIF.
    io_memory->stack_push( from_unsigned( lv_result ) ).

  ENDMETHOD.

  METHOD div_u.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_unsigned( ).
    DATA(lv_val2) = CAST zcl_wasm_i32( io_memory->stack_pop( ) )->get_unsigned( ).

    IF lv_val1 = 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i32.div_u, division by zero' ).
    ENDIF.

    io_memory->stack_push( from_unsigned( lv_val2 DIV lv_val1 ) ).

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
