CLASS zcl_wasm_f64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

    CLASS-METHODS from_float
      IMPORTING
        !iv_float TYPE f
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64.

    CLASS-METHODS floor_value
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CLASS-METHODS from_unsigned
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64
      RAISING
        zcx_wasm.

    TYPES ty_hex8 TYPE x LENGTH 8.
    CLASS-METHODS from_hex
      IMPORTING
        !iv_hex         TYPE ty_hex8
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64.

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

    CLASS-METHODS eq
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
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

    METHODS get_sign
      RETURNING
        VALUE(rv_negative) TYPE abap_bool.

    METHODS get_value
      RETURNING
        VALUE(rv_value) TYPE f
      RAISING
        zcx_wasm.

    CLASS-METHODS ne
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
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f64 IMPLEMENTATION.

  METHOD zif_wasm_value~human_readable_value.
    rv_string = |f64: { mv_value STYLE = SCIENTIFIC }|.
  ENDMETHOD.

  METHOD from_unsigned.
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = zcl_wasm_i64=>from_unsigned( iv_value )->get_signed( ).
    lv_hex8 = lv_int8.
    ro_value = from_hex( lv_hex8 ).
  ENDMETHOD.

  METHOD from_hex.
* webassembly is little endian
* javascript + open-abap is little endian

    DATA lv_f TYPE f.
    FIELD-SYMBOLS <lv_hex> TYPE x.


    ASSIGN lv_f TO <lv_hex> CASTING TYPE x.

    CASE cl_abap_char_utilities=>endian.
      WHEN 'L'.
        <lv_hex> = iv_hex.
      WHEN 'B'.
        <lv_hex> = zcl_wasm_binary_stream=>reverse_hex( iv_hex ).
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    ro_value = from_float( lv_f ).

  ENDMETHOD.

  METHOD gt.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f64 gt, expected at least two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_value( ) > lo_val2->get_value( ).
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD lt.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->mv_value > lo_val2->mv_value.
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD ge.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f64 gt, expected at least two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_value( ) >= lo_val2->get_value( ).
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD le.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'le, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->mv_value >= lo_val2->mv_value.
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD add.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val1->get_value( ) + lo_val2->get_value( ) ) ).

  ENDMETHOD.

  METHOD sub.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val2->get_value( ) - lo_val1->get_value( ) ) ).

  ENDMETHOD.

  METHOD mul.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val2->get_value( ) * lo_val1->get_value( ) ) ).

  ENDMETHOD.

  METHOD div.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( lo_val2->get_value( ) / lo_val1->get_value( ) ) ).

  ENDMETHOD.

  METHOD ne.

    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'ne, expected two variables on stack'.
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->mv_value <> lo_val2->mv_value.
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD get_sign.
    rv_negative = xsdbool( mv_value < 0 ).
  ENDMETHOD.

  METHOD get_value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD from_float.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_float.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.

    rv_type = zif_wasm_types=>c_value_type-f64.

  ENDMETHOD.

  METHOD floor_value.

    IF io_memory->mi_stack->get_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'f64 floor, expected at least one variables on stack'.
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    io_memory->mi_stack->push( from_float( floor( lo_val->mv_value ) ) ).

  ENDMETHOD.

  METHOD eq.

    ASSERT io_memory->mi_stack->get_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) )->mv_value.
    DATA(lv_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) )->mv_value.

    IF lv_val1 = lv_val2.
      io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( 1 ) ).
    ELSE.
      io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
