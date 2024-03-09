CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test1.

    " (module
    " (func (export "add") (result i64)
    " i64.const 0x7fffffffffffffff
    "   i64.const 1
    "   i64.add))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNhZGQAAAoSARAAQv///////////wBCAXwLAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'add' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i64( lt_values[ 1 ] ).

    " in hex = 0x8000000000000000
    cl_abap_unit_assert=>assert_equals(
      act = lo_value->get_signed( )
      exp = -9223372036854775808 ).
  ENDMETHOD.

ENDCLASS.
