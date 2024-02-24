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

    CLASS-METHODS gt
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
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f32 IMPLEMENTATION.

  METHOD from_float.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_value.
  ENDMETHOD.

  METHOD to_hex.
* https://gregstoll.com/~gregstoll/floattohex/

* todo
    CASE mv_value.
      WHEN 0.
        rv_hex = '00000000'.
      WHEN 1.
        rv_hex = '3F800000'.
      WHEN 2.
        rv_hex = '40000000'.
      WHEN 3.
        rv_hex = '40400000'.
      WHEN 25.
        rv_hex = '41C80000'.
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_wasm( text = |todo: zcl_wasm_f32, to_hex, { mv_value STYLE = SCIENTIFIC }| ).
    ENDCASE.

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
    DATA lv_bit     TYPE i.
    DATA lv_current TYPE i VALUE 1.

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
      RAISE EXCEPTION NEW zcx_wasm( text = 'f32 gt, expected at least two variables on stack' ).
    ENDIF.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    DATA(lv_result) = 0.
    IF lo_val1->get_value( ) > lo_val2->get_value( ).
      lv_result = 1.
    ENDIF.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

  METHOD square_root.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION NEW zcx_wasm( text = 'f32 sqrt, expected at least one variables on stack' ).
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_float( sqrt( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD floor_value.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION NEW zcx_wasm( text = 'f32 floor, expected at least one variables on stack' ).
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_float( floor( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD ceil_value.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION NEW zcx_wasm( text = 'f32 ceil, expected at least one variables on stack' ).
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_float( ceil( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD trunc_value.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION NEW zcx_wasm( text = 'f32 trunc, expected at least one variables on stack' ).
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_float( trunc( lo_val->get_value( ) ) ) ).

  ENDMETHOD.

  METHOD eq.

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lv_val1) = CAST zcl_wasm_f32( io_memory->stack_pop( ) )->mv_value.
    DATA(lv_val2) = CAST zcl_wasm_f32( io_memory->stack_pop( ) )->mv_value.

    IF lv_val1 = lv_val2.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( zcl_wasm_i32=>from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
