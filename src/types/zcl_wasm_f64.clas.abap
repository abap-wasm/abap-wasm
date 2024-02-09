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
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f64 IMPLEMENTATION.

  METHOD from_float.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_float.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_types=>c_value_type-f64.

  ENDMETHOD.

  METHOD floor_value.

    IF io_memory->stack_length( ) < 1.
      RAISE EXCEPTION NEW zcx_wasm( text = 'f64 floor, expected at least one variables on stack' ).
    ENDIF.

    DATA(lo_val) = CAST zcl_wasm_f64( io_memory->stack_pop( ) ).

    io_memory->stack_push( from_float( floor( lo_val->mv_value ) ) ).

  ENDMETHOD.
ENDCLASS.
