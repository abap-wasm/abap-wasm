CLASS zcl_wasm_f64 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wasm_value .

    METHODS constructor
      IMPORTING
        !iv_value TYPE f OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_value TYPE f .
ENDCLASS.

CLASS zcl_wasm_f64 IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD zif_wasm_value~get_type.

    rv_type = zcl_wasm_types=>c_value_type-f64.

  ENDMETHOD.
ENDCLASS.
