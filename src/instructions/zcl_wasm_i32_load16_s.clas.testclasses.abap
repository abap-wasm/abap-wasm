CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS negative FOR TESTING RAISING cx_static_check.
    METHODS negative_4242 FOR TESTING RAISING cx_static_check.
    METHODS hmm FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD negative.

" (module
"   (func (export "name") (result i32)
"     i32.const 0
"     i32.const -1
"     i32.store
"     i32.const 0
"     i32.load16_s)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBwgBBG5hbWUAAAoQAQ4AQQBBfzYCAEEALgEACwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -1 ).
  ENDMETHOD.

  METHOD negative_4242.

" (module
"   (func (export "name") (result i32)
"     i32.const 0
"     i32.const -4242
"     i32.store
"     i32.const 0
"     i32.load16_s)
"   (memory (;0;) 1)
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBwgBBG5hbWUAAAoRAQ8AQQBB7l42AgBBAC4BAAsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'name' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -4242 ).
  ENDMETHOD.

  METHOD hmm.

    " (module
    " (func $swap (param i32 i32)
    "   local.get 0
    "   local.get 1
    "   i32.store8
    "   local.get 0
    "   i32.const 1
    "   i32.add
    "   local.get 1
    "   i32.const 8
    "   i32.shr_u
    "   i32.store8)
    " (func (export "hmm") (param i32) (result i32)
    "   i32.const 0
    "   local.get 0
    "   call $swap
    "   i32.const 0
    "   i32.load16_s)
    " (memory (;0;) 1))

    DATA(lv_wasm) = `AGFzbQEAAAABCwJgAn9/AGABfwF/AwMCAAEFAwEAAQcHAQNobW0AAQomAhYAIAAgAToAACAAQQFqIAFBCHY6AAALDQBBACAAEABBAC4BAAsAFQRuYW1lAQcBAARzd2FwAgUCAAABAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export(
      iv_name       = 'hmm'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( -4242 ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    DATA(lo_value) = CAST zcl_wasm_i32( lt_values[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_value->mv_value
      exp = -4242 ).

  ENDMETHOD.

ENDCLASS.
