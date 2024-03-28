CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS negative FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD negative.

    " (module
    " (func (export "neg") (result i32)
    "   i32.const -4242
    "   i32.const 8
    "   i32.shr_u))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcHAQNuZWcAAAoKAQgAQe5eQQh2CwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'neg' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 16777199 ).

  ENDMETHOD.

ENDCLASS.
