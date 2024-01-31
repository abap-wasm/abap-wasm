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
        VALUE(rv_value) TYPE REF TO zcl_wasm_f32
      RAISING
        zcx_wasm.

    METHODS get_unsigned_i32
      RETURNING
        VALUE(rv_value) TYPE int8
      RAISING
        zcx_wasm.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f32 IMPLEMENTATION.

  METHOD from_unsigned_i32.
    RAISE EXCEPTION NEW zcx_wasm( text = |todo: zcl_wasm_f32, from_unsigned_i32| ).
  ENDMETHOD.

  METHOD get_unsigned_i32.
    RAISE EXCEPTION NEW zcx_wasm( text = |todo: zcl_wasm_f32, get_unsigned_i32| ).
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_types=>c_value_type-f32.

  ENDMETHOD.
ENDCLASS.
