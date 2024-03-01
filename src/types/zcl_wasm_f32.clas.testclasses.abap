CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS from_unsigned_i32_zero FOR TESTING RAISING cx_static_check.
    METHODS from_unsigned_i32_1 FOR TESTING RAISING cx_static_check.
    METHODS from_unsigned_i32_2 FOR TESTING RAISING cx_static_check.

    METHODS to_hex_0 FOR TESTING RAISING cx_static_check.
    METHODS to_hex_25 FOR TESTING RAISING cx_static_check.
    METHODS to_hex_12_375 FOR TESTING RAISING cx_static_check.
    METHODS to_hex_0_0625 FOR TESTING RAISING cx_static_check.

    METHODS get_unsigned_32 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD from_unsigned_i32_zero.
    DATA(lv_float) = zcl_wasm_f32=>from_unsigned_i32( 0 )->get_value( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_float
      exp = 0 ).
  ENDMETHOD.

  METHOD from_unsigned_i32_1.
    " unsigned 1082130432 = in hex 40800000
    DATA(lv_float) = zcl_wasm_f32=>from_unsigned_i32( 1082130432 )->get_value( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_float
      exp = 4 ).
  ENDMETHOD.

  METHOD from_unsigned_i32_2.
    DATA(lv_float) = zcl_wasm_f32=>from_unsigned_i32( 1159892992 )->get_value( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_float
      exp = 2601 ).
  ENDMETHOD.

  METHOD to_hex_0.
    DATA(lv_hex) = zcl_wasm_f32=>from_float( 0 )->to_hex( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_hex
      exp = '00000000' ).
  ENDMETHOD.

  METHOD to_hex_25.
    DATA(lv_hex) = zcl_wasm_f32=>from_float( 25 )->to_hex( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_hex
      exp = '41C80000' ).
  ENDMETHOD.

  METHOD get_unsigned_32.
    DATA(lv_unsigned) = zcl_wasm_f32=>from_float( 3 )->get_unsigned_i32( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_unsigned
      exp = 1077936128 ).
  ENDMETHOD.

  METHOD to_hex_12_375.
    DATA(lv_hex) = zcl_wasm_f32=>from_float( '12.375' )->to_hex( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_hex
      exp = '41460000' ).
  ENDMETHOD.

  METHOD to_hex_0_0625.
    DATA(lv_hex) = zcl_wasm_f32=>from_float( '0.0625' )->to_hex( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_hex
      exp = '3D800000' ).
  ENDMETHOD.

ENDCLASS.
