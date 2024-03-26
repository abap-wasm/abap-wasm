CLASS zcl_wasm_f64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

    CLASS-METHODS get_positive_infinity
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64.
    CLASS-METHODS get_negative_infinity
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64.
    CLASS-METHODS get_nan
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64.

    CLASS-METHODS from_float
      IMPORTING
        !iv_float       TYPE f
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_f64.

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

    METHODS get_unsigned
      RETURNING
        VALUE(rv_value) TYPE string
      RAISING
        zcx_wasm.

    METHODS get_hex
      RETURNING
        VALUE(rv_value) TYPE ty_hex8
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

    CONSTANTS: BEGIN OF gc_special_hex,
                 positive_infinity TYPE ty_hex8 VALUE '7FF0000000000000',
                 negative_infinity TYPE ty_hex8 VALUE 'FFF0000000000000',
                 nan               TYPE ty_hex8 VALUE '7FF8000000000000',
               END OF gc_special_hex.

    TYPES ty_special TYPE i.
    CONSTANTS: BEGIN OF gc_special,
                 positive_infinity TYPE ty_special VALUE 1,
                 negative_infinity TYPE ty_special VALUE 2,
                 nan               TYPE ty_special VALUE 3,
               END OF gc_special.
    METHODS get_special
      RETURNING VALUE(rv_special) TYPE ty_special.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_special TYPE ty_special.
    DATA mv_value TYPE f .
ENDCLASS.



CLASS zcl_wasm_f64 IMPLEMENTATION.


  METHOD from_float.
    ro_value = NEW #( ).
    ro_value->mv_value = iv_float.
  ENDMETHOD.


  METHOD from_hex.
* webassembly is little endian
* javascript + open-abap is little endian

    DATA lv_f TYPE f.
    FIELD-SYMBOLS <lv_hex> TYPE x.


    IF iv_hex = gc_special_hex-positive_infinity.
      ro_value = get_positive_infinity( ).
      RETURN.
    ELSEIF iv_hex = gc_special_hex-negative_infinity.
      ro_value = get_negative_infinity( ).
      RETURN.
    ELSEIF iv_hex = gc_special_hex-nan.
      ro_value = get_nan( ).
      RETURN.
    ENDIF.

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


  METHOD from_unsigned.
    DATA lv_int8 TYPE int8.
    DATA lv_hex8 TYPE x LENGTH 8.
    lv_int8 = zcl_wasm_i64=>from_unsigned( iv_value )->get_signed( ).
    lv_hex8 = lv_int8.
    ro_value = from_hex( lv_hex8 ).
  ENDMETHOD.


  METHOD get_hex.
    FIELD-SYMBOLS <lv_hex> TYPE x.

    IF mv_special <> 0.
      CASE mv_special.
        WHEN gc_special-positive_infinity.
          rv_value = gc_special_hex-positive_infinity.
        WHEN gc_special-negative_infinity.
          rv_value = gc_special_hex-negative_infinity.
        WHEN gc_special-nan.
          rv_value = gc_special_hex-nan.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
      RETURN.
    ENDIF.

    ASSIGN mv_value TO <lv_hex> CASTING TYPE x.
    rv_value = <lv_hex>.
  ENDMETHOD.


  METHOD get_nan.
* todo, singleton?
    ro_value = NEW #( ).
    ro_value->mv_special = gc_special-nan.
  ENDMETHOD.


  METHOD get_negative_infinity.
* todo, singleton?
    ro_value = NEW #( ).
    ro_value->mv_special = gc_special-negative_infinity.
  ENDMETHOD.


  METHOD get_positive_infinity.
* todo, singleton?
    ro_value = NEW #( ).
    ro_value->mv_special = gc_special-positive_infinity.
  ENDMETHOD.


  METHOD get_sign.
    IF mv_special = gc_special-negative_infinity.
      rv_negative = abap_true.
    ELSE.
      rv_negative = xsdbool( mv_value < 0 ).
    ENDIF.
  ENDMETHOD.


  METHOD get_special.
    rv_special = mv_special.
  ENDMETHOD.


  METHOD get_unsigned.
    DATA lv_hex  TYPE ty_hex8.
    DATA lv_int8 TYPE int8.
    lv_hex = get_hex( ).
    lv_int8 = lv_hex.
    rv_value = zcl_wasm_i64=>from_signed( lv_int8 )->get_unsigned( ).
  ENDMETHOD.


  METHOD get_value.
    IF mv_special IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'f64: error trying to get value for special'.
    ENDIF.

    rv_value = mv_value.
  ENDMETHOD.


  METHOD zif_wasm_value~get_type.
    rv_type = zif_wasm_types=>c_value_type-f64.
  ENDMETHOD.


  METHOD zif_wasm_value~human_readable_value.
    rv_string = |f64: { mv_value STYLE = SCIENTIFIC }|.
  ENDMETHOD.
ENDCLASS.
