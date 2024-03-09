CLASS ltcl_test DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS breakvalue FOR TESTING RAISING cx_static_check.

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
      act = CAST zcl_wasm_i32( lt_values[ 1 ] )->get_signed( )
      exp = 18 ).

  ENDMETHOD.

ENDCLASS.
