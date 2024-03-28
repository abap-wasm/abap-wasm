CLASS zcl_wasm_f32 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

    CLASS-METHODS from_unsigned_i32
      IMPORTING
        !iv_value       TYPE int8
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f32
      RAISING
        zcx_wasm.

    CLASS-METHODS from_float
      IMPORTING
        !iv_value       TYPE f
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f32
      RAISING
        zcx_wasm.

    METHODS get_unsigned_i32
      RETURNING
        VALUE(rv_value) TYPE int8
      RAISING
        zcx_wasm.

    TYPES ty_hex4 TYPE x LENGTH 4.
    METHODS to_hex
      RETURNING
        VALUE(rv_hex) TYPE ty_hex4
      RAISING
        zcx_wasm.

    METHODS get_value
      RETURNING
        VALUE(rv_value) TYPE f
      RAISING
        zcx_wasm.

    CLASS-METHODS add
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS sub
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS mul
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS div
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS square_root
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS floor_value
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS ceil_value
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS trunc_value
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS eq
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS le
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS lt
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS ne
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS gt
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS ge
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    METHODS get_sign
      RETURNING
        VALUE(rv_negative) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f32 IMPLEMENTATION.

  METHOD zif_wasm_value~human_readable_value.
    rv_string = |f32: { mv_value STYLE = SCIENTIFIC }|.
  ENDMETHOD.

  METHOD get_sign.
    rv_negative = xsdbool( mv_value < 0 ).
  ENDMETHOD.

  METHOD from_float.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_value.
  ENDMETHOD.

  METHOD to_hex.
* https://gregstoll.com/~gregstoll/floattohex/
* https://en.wikipedia.org/wiki/Single-precision_floating-point_format
* https://webassembly.github.io/spec/core/exec/numerics.html#rounding

    DATA lv_fraction TYPE f.
    DATA lv_integer  TYPE i.
    DATA lv_bit      TYPE c LENGTH 1.
    DATA lv_hex1     TYPE x LENGTH 1.
    DATA lv_exponent TYPE i.

    DATA lv_integer_bits  TYPE string.
    DATA lv_fraction_bits TYPE string.

    CASE mv_value.
      WHEN 0.
        rv_hex = '00000000'.
      WHEN OTHERS.
        lv_integer = trunc( abs( mv_value ) ).
        IF lv_integer = 0.
          lv_integer_bits = '0'.
        ELSE.
          WHILE lv_integer > 0.
            lv_bit = lv_integer MOD 2.
            lv_integer = lv_integer DIV 2.
            lv_integer_bits = lv_bit && lv_integer_bits.
          ENDWHILE.
        ENDIF.

        lv_fraction = frac( mv_value ).
        WHILE lv_fraction > 0 AND strlen( lv_fraction_bits ) < 23.
          lv_fraction = lv_fraction * 2.
          IF lv_fraction >= 1.
            lv_fraction = lv_fraction - 1.
            lv_fraction_bits = lv_fraction_bits && '1'.
          ELSE.
            lv_fraction_bits = lv_fraction_bits && '0'.
          ENDIF.
        ENDWHILE.
        IF lv_fraction_bits = ''.
          lv_fraction_bits = '0'.
        ENDIF.
*        WRITE: / 'fraction bits:', lv_fraction_bits.

* todo, moving decimal point to the right for lower numbers
        IF lv_integer_bits <> '0'.
          lv_exponent = 127 + strlen( lv_integer_bits ) - 1.
        ELSE.
          FIND FIRST OCCURRENCE OF '1' IN lv_fraction_bits MATCH OFFSET lv_exponent.
          lv_exponent = lv_exponent + 1.
          lv_fraction_bits = lv_fraction_bits+lv_exponent.
          lv_exponent = 127 - lv_exponent.
*          WRITE: / 'fraction bits, adjusted:', lv_fraction_bits.
        ENDIF.
        IF lv_exponent > 255.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |exponent too large: { lv_exponent }|.
        ENDIF.
*        WRITE: / 'exponent:', lv_exponent.
        lv_hex1 = lv_exponent.

        lv_fraction_bits = lv_integer_bits+1 && lv_fraction_bits.

        IF mv_value < 0.
          SET BIT 1 OF rv_hex TO 1.
        ENDIF.

        DO 8 TIMES.
          GET BIT sy-index OF lv_hex1 INTO lv_bit.
          DATA(lv_offset) = sy-index + 1.
          SET BIT lv_offset OF rv_hex TO lv_bit.
        ENDDO.

        IF strlen( lv_fraction_bits ) > 23.
          lv_fraction_bits = lv_fraction_bits(23).
        ENDIF.
        DO strlen( lv_fraction_bits ) TIMES.
          lv_offset = sy-index - 1.
          lv_bit = lv_fraction_bits+lv_offset(1).
          lv_offset = sy-index + 9.
          SET BIT lv_offset OF rv_hex TO lv_bit.
        ENDDO.

*        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |todo: zcl_wasm_f32, to_hex, { mv_value STYLE = SCIENTIFIC }| ).
    ENDCASE.

  ENDMETHOD.

  METHOD get_value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD from_unsigned_i32.
    DATA lv_hex TYPE x LENGTH 4.
    DATA(lv_int) = zcl_wasm_i32=>from_unsigned( iv_value )->mv_value.
    lv_hex = lv_int.
    ro_value = NEW #( ).
    lv_hex = zcl_wasm_binary_stream=>reverse_hex( lv_hex ).
    ro_value->mv_value = NEW zcl_wasm_binary_stream( lv_hex )->shift_f32( ).
  ENDMETHOD.

  METHOD get_unsigned_i32.
    DATA lv_bit     TYPE i.
    DATA lv_current TYPE int8 VALUE 1.

    IF mv_value = 0.
      rv_value = 0.
    ELSE.
      DATA(lv_hex) = to_hex( ).

      DO 32 TIMES.
        GET BIT 33 - sy-index OF lv_hex INTO lv_bit.
        IF lv_bit = 1.
          rv_value = rv_value + lv_current.
        ENDIF.
        lv_current = lv_current * 2.
      ENDDO.

    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.
    rv_type = zif_wasm_types=>c_value_type-f32.
  ENDMETHOD.

  METHOD add.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val1->get_value( ) + lo_val2->get_value( ) ) ).

  ENDMETHOD.

  METHOD sub.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val2->get_value( ) - lo_val1->get_value( ) ) ).

  ENDMETHOD.

  METHOD mul.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val2->get_value( ) * lo_val1->get_value( ) ) ).

  ENDMETHOD.

  METHOD div.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val2->get_value( ) / lo_val1->get_value( ) ) ).

  ENDMETHOD.

  METHOD gt.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f32 gt, expected at least two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_value( ) > lo_val2->get_value( ).
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD ge.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f32 gt, expected at least two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_value( ) >= lo_val2->get_value( ).
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD square_root.

    IF io_memory->mi_stack->get_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f32 sqrt, expected at least one variables on stack'.
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( sqrt( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD floor_value.

    IF io_memory->mi_stack->get_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f32 floor, expected at least one variables on stack'.
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( floor( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD ceil_value.

    IF io_memory->mi_stack->get_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f32 ceil, expected at least one variables on stack'.
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( ceil( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD trunc_value.

    IF io_memory->mi_stack->get_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f32 trunc, expected at least one variables on stack'.
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( trunc( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD eq.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) )->mv_value.
    DATA(lv_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) )->mv_value.

    IF lv_val1 = lv_val2.
      io_memory->mi_stack->push( zcl_wasm_i32=>gc_one ).
    ELSE.
      io_memory->mi_stack->push( zcl_wasm_i32=>gc_zero ).
    ENDIF.

  ENDMETHOD.

  METHOD le.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'le, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->mv_value >= lo_val2->mv_value.
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD lt.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->mv_value > lo_val2->mv_value.
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD ne.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'ne, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->mv_value <> lo_val2->mv_value.
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

ENDCLASS.
