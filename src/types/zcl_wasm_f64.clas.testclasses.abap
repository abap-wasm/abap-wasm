CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS from_hex FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD from_hex.
    DATA(lv_float) = zcl_wasm_f64=>from_hex( '0000000000000040' )->get_value( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_float
      exp = 2 ).
  ENDMETHOD.

ENDCLASS.
