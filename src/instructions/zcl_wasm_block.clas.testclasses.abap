CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD empty.

    " (module
    " (func (export "empty")
    "   block
    "   end
    "   block
    "   end))

    DATA(lv_wasm) = 'AGFzbQEAAAABBAFgAAADAgEABwkBBWVtcHR5AAAKCgEIAAJACwJACwsACgRuYW1lAgMBAAA='.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    li_wasm->execute_function_export( 'empty' ).
  ENDMETHOD.

ENDCLASS.
