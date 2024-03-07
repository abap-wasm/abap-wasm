CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS sub_0_0 FOR TESTING RAISING cx_static_check.
    METHODS sub_1_1 FOR TESTING RAISING cx_static_check.
    METHODS sub_n1_n1 FOR TESTING RAISING cx_static_check.
    METHODS sub_n1_1 FOR TESTING RAISING cx_static_check.
    METHODS sub_1_5 FOR TESTING RAISING cx_static_check.
    METHODS sub_5_1 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD sub_0_0.

" (module
"   (func (export "sub") (result i64)
"   i64.const 0
"   i64.const 0
"   i64.sub
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNzdWIAAAoJAQcAQgBCAH0LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 0 ).
  ENDMETHOD.

  METHOD sub_1_1.

" (module
"   (func (export "sub") (result i64)
"   i64.const 1
"   i64.const 1
"   i64.sub
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNzdWIAAAoJAQcAQgFCAX0LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 0 ).
  ENDMETHOD.

  METHOD sub_n1_n1.

" (module
"   (func (export "sub") (result i64)
"   i64.const -1
"   i64.const -1
"   i64.sub
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNzdWIAAAoJAQcAQn9Cf30LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 0 ).
  ENDMETHOD.

  METHOD sub_n1_1.

" (module
"   (func (export "sub") (result i64)
"   i64.const -1
"   i64.const 1
"   i64.sub
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNzdWIAAAoJAQcAQn9CAX0LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = -2 ).
  ENDMETHOD.

  METHOD sub_1_5.

" (module
"   (func (export "sub") (result i64)
"   i64.const 1
"   i64.const 5
"   i64.sub
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNzdWIAAAoJAQcAQgFCBX0LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = -4 ).
  ENDMETHOD.

  METHOD sub_5_1.

" (module
"   (func (export "sub") (result i64)
"   i64.const 5
"   i64.const 1
"   i64.sub
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF+AwIBAAcHAQNzdWIAAAoJAQcAQgVCAX0LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'sub' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i64( lt_values[ 1 ] )->get_signed( )
      exp = 4 ).
  ENDMETHOD.

ENDCLASS.
