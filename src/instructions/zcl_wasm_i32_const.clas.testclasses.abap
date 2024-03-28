CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD empty.

    " (module
    " (func (export "minus2") (result i32)
    "   i32.const -2))

    " minus 2 becomes 7E in hex

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZtaW51czIAAAoGAQQAQX4LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'minus2' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = -2 ).
  ENDMETHOD.

ENDCLASS.
