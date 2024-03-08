CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.
    METHODS multi FOR TESTING RAISING cx_static_check.

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

ENDCLASS.
