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

  METHOD zif_wasm_value~get_type.

    rv_type = zif_wasm_types=>c_value_type-i32.

  ENDMETHOD.

ENDCLASS.
