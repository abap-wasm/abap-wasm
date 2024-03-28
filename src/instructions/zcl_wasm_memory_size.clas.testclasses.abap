CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS basic FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD basic.

    " (module
    "   (type (;0;) (func (result i32)))
    "   (func (;0;) (type 0) (result i32)
    "     memory.size)
    "   (memory (;0;) 1 1)
    "   (export "memsize" (func 0))
    "   (data (;0;) (i32.const 0) "x"))

    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAUEAQEBAQcLAQdtZW1zaXplAAAKBgEEAD8ACwsHAQBBAAsBeA==`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'memsize' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_values )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.
