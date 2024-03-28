CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS breakvalue FOR TESTING RAISING cx_static_check.
    METHODS loopbr FOR TESTING RAISING cx_static_check.
    METHODS loopbr2 FOR TESTING RAISING cx_static_check.
    METHODS loopbr3 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD breakvalue.

" (module
"   (func (export "breakvalue") (result i32)
"     block (result i32)  ;; label = @1
"       i32.const 0
"       loop (param i32)  ;; label = @2
"         block  ;; label = @3
"           i32.const 18
"           br 2 (;@1;)
"         end
"         i32.const 20
"         br 0 (;@2;)
"       end
"       i32.const 19
"     end))

    DATA(lv_wasm) = `AGFzbQEAAAABCQJgAAF/YAF/AAMCAQAHDgEKYnJlYWt2YWx1ZQAAChkBFwACf0EAAwECQEESDAILQRQMAAtBEwsLAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'breakvalue' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 18 ).

  ENDMETHOD.

  METHOD loopbr.

" (module
"   (func (export "loop") (result i32)
"     loop (result i32)
"       i32.const 2
"       br 1
"     end))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcIAQRsb29wAAAKCwEJAAN/QQIMAQsLAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'loop' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 2 ).

  ENDMETHOD.

  METHOD loopbr2.

" (module
"   (func (export "loop") (result i32)
"     loop (result i32)
"       i32.const 7
"       br 1
"     end
"     i32.const 1
"     i32.add))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcIAQRsb29wAAAKDgEMAAN/QQcMAQtBAWoLAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'loop' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 7 ).

  ENDMETHOD.

  METHOD loopbr3.

" (module
"   (func (export "loop") (result i32)
"     block (result i32)
"       loop (result i32)
"         i32.const 7
"         br 1
"       end
"     end
"     i32.const 1
"     i32.add))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcIAQRsb29wAAAKEQEPAAJ/A39BBwwBCwtBAWoLAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'loop' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 8 ).

  ENDMETHOD.

ENDCLASS.
