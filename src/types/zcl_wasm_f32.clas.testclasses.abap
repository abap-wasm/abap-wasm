CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS from_unsigned_i32_zero FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD from_unsigned_i32_zero.

    DATA(lo_value) = zcl_wasm_f32=>from_unsigned_i32( 0 ).
    DATA(lv_float) = lo_value->get_value( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_float
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
