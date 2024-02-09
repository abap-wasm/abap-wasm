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

    CLASS-METHODS gt
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f32 IMPLEMENTATION.

  METHOD from_float.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_value.
  ENDMETHOD.

  METHOD get_value.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD from_unsigned_i32.
    DATA lv_hex TYPE x LENGTH 4.
    DATA(lv_int) = zcl_wasm_i32=>from_unsigned( iv_value )->get_signed( ).
    lv_hex = lv_int.
    ro_value = NEW #( ).
    ro_value->mv_value = NEW zcl_wasm_binary_stream( lv_hex )->shift_f32( ).
  ENDMETHOD.

  METHOD get_unsigned_i32.
    IF mv_value = 0.
      RETURN.
    ENDIF.

    RAISE EXCEPTION NEW zcx_wasm( text = |todo: zcl_wasm_f32, get_unsigned_i32| ).
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.
    rv_type = zcl_wasm_types=>c_value_type-f32.
  ENDMETHOD.

  METHOD add.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_float( lo_val1->get_value( ) + lo_val2->get_value( ) ) ).

  ENDMETHOD.

  METHOD gt.

    IF io_memory->stack_length( ) < 2.
      RAISE EXCEPTION NEW zcx_wasm( text = 'f32 gt, expected two variables on stack' ).
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_value( ) > lo_val2->get_value( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

ENDCLASS.
