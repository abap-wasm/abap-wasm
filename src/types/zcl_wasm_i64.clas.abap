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

* only used in testclasses?
* todo: this should really be a unsigned to hex thing? it will also be used for f64 tests
    METHODS get_unsigned
      RETURNING
        VALUE(rv_value) TYPE string
      RAISING
        zcx_wasm.

    METHODS get_signed
      RETURNING
        VALUE(rv_value) TYPE int8
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
* todo, use packed? nah int8 is long enough, but need to handle unsigned
    DATA mv_value TYPE int8 .
ENDCLASS.

CLASS zcl_wasm_i64 IMPLEMENTATION.

  METHOD get_signed.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD zif_wasm_value~human_readable_value.
    rv_string = |i64: { mv_value }|.
  ENDMETHOD.


  METHOD from_unsigned.
* https://www.rapidtables.com/convert/number/hex-to-decimal.html
    IF iv_value CN '-0123456789'.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64, from_unsigned, unexpected value'.
    ENDIF.

    ro_value = NEW #( ).

    CASE iv_value.
      WHEN '18446744073709551615'.
        ro_value->mv_value = -1.
      WHEN '18446744073709551614'.
        ro_value->mv_value = -2.
      WHEN '18446744073709551613'.
        ro_value->mv_value = -3.
      WHEN '18446744073709551612'.
        ro_value->mv_value = -4.
      WHEN '18446744073709551611'.
        ro_value->mv_value = -5.
      WHEN '18446744073709551609'.
        ro_value->mv_value = -7.
      WHEN '18446744073709551601'.
        ro_value->mv_value = -15.
      WHEN '18446744073709547374'.
        ro_value->mv_value = -4242.
      WHEN '18446744073709451616'.
        ro_value->mv_value = -100000.
      WHEN '18446744073667127374'.
        ro_value->mv_value = -42424242.
      WHEN '12297829381041378645'.
        ro_value->mv_value = -6148914692668172971.
      WHEN '13835058055282163712'.
        ro_value->mv_value = -4611686018427387904.
      WHEN '4611686018427387904'.
        ro_value->mv_value = 4611686018427387904.
      WHEN '1152921504606846976'.
        ro_value->mv_value = 1152921504606846976.
      WHEN '9223372036854775807'.
        ro_value->mv_value = 9223372036854775807.
      WHEN '9223372036854775808'.
        ro_value->mv_value = -9223372036854775808.
      WHEN '1311768467463733248'.
        ro_value->mv_value = 1311768467463733248.
      WHEN '3458764513820540928'.
        ro_value->mv_value = 3458764513820540928.
      WHEN '5764607523034234880'.
        ro_value->mv_value = 5764607523034234880.
      WHEN '11529215046068469760'.
        ro_value->mv_value = -6917529027641081856.
      WHEN '18446744073709551605'.
        ro_value->mv_value = -11.
      WHEN '8070450532247928832'.
        ro_value->mv_value = 8070450532247928832.
      WHEN '16140901064495857664'.
        ro_value->mv_value = -2305843009213693952.
      WHEN '10371807465568210928'.
        ro_value->mv_value = -8074936608141340688.
      WHEN '9223372036854775809'.
        ro_value->mv_value = -9223372036854775807.
      WHEN '12379718583323101902'.
        ro_value->mv_value = -6067025490386449714.
      WHEN '12370766947463011818'.
        ro_value->mv_value = -6075977126246539798.
      WHEN '9227875636482146304'.
        ro_value->mv_value = -9218868437227405312.
      WHEN '13826050856027422720'.
        ro_value->mv_value = -4620693217682128896.
      WHEN '18442240474082181120'.
        ro_value->mv_value = -4503599627370496.
    ENDCASE.

    IF ro_value->mv_value <> 0.
      RETURN.
    ENDIF.

    IF strlen( iv_value ) > 18.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |i64, from_unsigned, value too long, todo, "{ iv_value }"|.
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
      CASE mv_value.
        WHEN -1.
          rv_value = '18446744073709551615'.
        WHEN -2.
          rv_value = '18446744073709551614'.
        WHEN -3.
          rv_value = '18446744073709551613'.
        WHEN -4.
          rv_value = '18446744073709551612'.
        WHEN -5.
          rv_value = '18446744073709551611'.
        WHEN -15.
          rv_value = '18446744073709551601'.
        WHEN -128.
          rv_value = '18446744073709551488'.
        WHEN -4242.
          rv_value = '18446744073709547374'.
        WHEN -10000.
          rv_value = '18446744073709541616'.
        WHEN -32768.
          rv_value = '18446744073709518848'.
        WHEN -42424242.
          rv_value = '18446744073667127374'.
        WHEN -9223372036854775808.
          rv_value = '9223372036854775808'.
        WHEN -9223372036854775807.
          rv_value = '9223372036854775809'.
        WHEN -4611686018427387904.
          rv_value = '13835058055282163712'.
        WHEN -790564704.
          rv_value = '18446744072918986912'.
        WHEN -6075977126246539798.
          rv_value = '12370766947463011818'.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |i64, todo get_unsigned, value is negative: { mv_value }|.
      ENDCASE.
      RETURN.
    ENDIF.

    rv_value = |{ mv_value }|.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.
    rv_type = zif_wasm_types=>c_value_type-i64.
  ENDMETHOD.

ENDCLASS.
