CLASS zcl_wasm_i64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

* todo, from_signed() and from_unsigned() methods
    METHODS constructor
      IMPORTING
        !iv_value TYPE int8 .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE int8 .
ENDCLASS.

CLASS zcl_wasm_i64 IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_types=>c_value_type-i64.

  ENDMETHOD.

ENDCLASS.
