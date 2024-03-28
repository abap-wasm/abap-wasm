CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS asifcond FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD asifcond.

" (module
"   (func (export "asifcond") (result i32)
"     block (result i32)  ;; label = @1
"       i32.const 1
"       i32.const 0
"       br_if 0 (;@1;)
"       if (result i32)  ;; label = @2
"         i32.const 2
"       else
"         i32.const 3
"       end
"     end))


    DATA(lv_wasm) = `AGFzbQEAAAABBQFgAAF/AwIBAAcMAQhhc2lmY29uZAAAChUBEwACf0EBQQANAAR/QQIFQQMLCwsACgRuYW1lAgMBAAA=`.

    DATA(li_wasm) = zcl_wasm=>create_with_base64( lv_wasm ).

    DATA(lt_values) = li_wasm->execute_function_export( 'asifcond' ).

    cl_abap_unit_assert=>assert_not_initial( lt_values ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_values[ 1 ]->get_type( )
      exp = zif_wasm_types=>c_value_type-i32 ).

    cl_abap_unit_assert=>assert_equals(
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->mv_value
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.
