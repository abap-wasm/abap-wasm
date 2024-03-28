CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.
    METHODS multi FOR TESTING RAISING cx_static_check.
    METHODS nested_unwind FOR TESTING RAISING cx_static_check.
    METHODS select_mid FOR TESTING RAISING cx_static_check.
    METHODS blocks FOR TESTING RAISING cx_static_check.
    METHODS blocks_with_br FOR TESTING RAISING cx_static_check.
    METHODS blocks_with_br_after FOR TESTING RAISING cx_static_check.
    METHODS blocks_longer_br FOR TESTING RAISING cx_static_check.
    METHODS return_block FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD empty.

    " (module
    " (func (export "empty")
    "   block
    "   end
    "   block
    "   end))

    DATA(lv_wasm) = `AGFzbQEAAAABBAFgAAADAgEABwkBBWVtcHR5AAAKCgEIAAJACwJACwsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    li_wasm->execute_function_export( 'empty' ).
  ENDMETHOD.

  METHOD multi.

" (module
"   (func (export "multi") (result i32 i32 i64)
"     block (result i32 i32 i64)  ;; label = @1
"       i32.const 1
"       i32.const 1
"       i64.const 1
"       br 0 (;@1;)
"       i32.const 1
"       i32.const 1
"       i64.const 1
"     end
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBwFgAAN/f34DAgEABwkBBW11bHRpAAAKFQETAAIAQQFBAUIBDABBAUEBQgELCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'multi' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 3 ).

  ENDMETHOD.

  METHOD nested_unwind.

" (module
"   (func (export "nested_unwind") (result i32)
"     block (result i32)  ;; label = @1
"       i32.const 3
"       block  ;; label = @2
"         i64.const 1
"         i32.const 9
"         br 1 (;@1;)
"       end
"     end
" ))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcRAQ1uZXN0ZWRfdW53aW5kAAAKEgEQAAJ/QQMCQEIBQQkMAQsLCwAKBG5hbWUCAwEAAA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'nested_unwind' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 9 ).

  ENDMETHOD.

  METHOD select_mid.

" (module
"   (func (export "selectmid") (result i32)
"     i32.const 2
"     block (result i32)  ;; label = @1
"       i32.const 1
"     end
"     i32.const 3
"     select))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcNAQlzZWxlY3RtaWQAAAoOAQwAQQICf0EBC0EDGwsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'selectmid' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 2 ).

  ENDMETHOD.

  METHOD blocks.

" (module
"   (func (export "blocks") (result i32)
"     block
"       block (result i32)
"         i32.const 1
"         block
"         end
"       end
"       return
"     end
"     i32.const 2
"   )
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZibG9ja3MAAAoSARAAAkACf0EBAkALCw8LQQILAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'blocks' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 1 ).

  ENDMETHOD.

  METHOD blocks_with_br.

" (module
"   (func (export "blocks") (result i32)
"     block
"       block (result i32)
"         i32.const 1
"         block
"         end
"       end
"       return
"     end
"     i32.const 2
"   )
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZibG9ja3MAAAoUARIAAkACf0EBAkAMAAsLDwtBAgsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'blocks' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 1 ).

  ENDMETHOD.

  METHOD blocks_with_br_after.

" (module
"   (func (export "blocks") (result i32)
"     block
"       block (result i32)
"         block
"           br 0
"         end
"         i32.const 1
"       end
"       return
"     end
"     i32.const 2
"   )
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZibG9ja3MAAAoUARIAAkACfwJADAALQQELDwtBAgsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'blocks' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 1 ).

  ENDMETHOD.

  METHOD blocks_longer_br.

" (module
"   (func (export "blocks") (result i32)
"     block
"       block (result i32)
"         block
"           br 2
"         end
"         i32.const 1
"       end
"       return
"     end
"     i32.const 2
"   )
" )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZibG9ja3MAAAoUARIAAkACfwJADAILQQELDwtBAgsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'blocks' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 2 ).

  ENDMETHOD.

  METHOD return_block.

"  (module
"    (func (export "blocks") (result i32)
"      block
"        block (result i64)
"          i32.const 4
"          return
"          i64.const 2
"        end
"        drop
"      end
"      i32.const 1
"    )
"  )

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcKAQZibG9ja3MAAAoSARAAAkACfkEED0ICCxoLQQELAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'blocks' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 4 ).

  ENDMETHOD.

ENDCLASS.
