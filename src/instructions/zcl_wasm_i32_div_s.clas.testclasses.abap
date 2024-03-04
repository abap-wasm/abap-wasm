CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

    " (module
    " (func (export "div_s") (result i32)
    "   i32.const -2147483648
    "   i32.const -1
    "   i32.div_s))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcJAQVkaXZfcwAACg0BCwBBgICAgHhBf20LAAoEbmFtZQIDAQAA`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    TRY.
        li_wasm->execute_function_export( 'div_s' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_wasm.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
