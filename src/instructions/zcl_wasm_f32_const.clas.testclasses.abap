CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS three FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD three.

    " (module
    " (func (export "three") (result f32)
    "   f32.const 0x1.8p+1))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF9AwIBAAcJAQV0aHJlZQAACgkBBwBDAABAQAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'three' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_f32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_value( )
      exp = 3 ).
  ENDMETHOD.

ENDCLASS.
