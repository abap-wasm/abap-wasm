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

    METHODS get_unsigned
      RETURNING
        VALUE(rv_value) TYPE string
      RAISING
        zcx_wasm.

    CLASS-METHODS eqz
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE int8 .
ENDCLASS.

CLASS zcl_wasm_i64 IMPLEMENTATION.

  METHOD from_unsigned.
    IF iv_value CN '-0123456789'.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i64, from_unsigned, unexpected value' ).
    ENDIF.

    IF strlen( iv_value ) > 8.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i64, from_unsigned, value too long, todo' ).
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
      RAISE EXCEPTION NEW zcx_wasm( text = 'i64, get_unsigned, value is negative' ).
    ENDIF.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.
    rv_type = zcl_wasm_types=>c_value_type-i64.
  ENDMETHOD.

  METHOD eqz.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION NEW zcx_wasm( text = 'i64, eqz, expected value on stack' ).
    ENDIF.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->stack_pop( ) )->mv_value.

    IF lv_val1 = 0.
      io_memory->stack_push( from_signed( 1 ) ).
    ELSE.
      io_memory->stack_push( from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
