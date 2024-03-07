CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS neg FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

" (module
"   (func (export "shr_s") (result i64)
"   i64.const 1
"   i64.const -1
"   i64.shr_s
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcJAQVzaHJfcwAACgkBBwBCAUJ/hwsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'shr_s' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 0 ).
  ENDMETHOD.

  METHOD neg.

" (module
"   (func (export "shr_s") (result i64)
"   i64.const -1
"   i64.const 1
"   i64.shr_s
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcJAQVzaHJfcwAACgkBBwBCf0IBhwsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'shr_s' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = -1 ).
  ENDMETHOD.

ENDCLASS.
