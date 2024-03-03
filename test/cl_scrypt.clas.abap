CLASS cl_scrypt DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING VALUE(rv_json) TYPE string
      RAISING zcx_wasm.
ENDCLASS.

CLASS cl_scrypt IMPLEMENTATION.

  METHOD run.

    DATA lv_hex TYPE xstring.

    WRITE '@KERNEL const fs = await import("fs");'.
    WRITE '@KERNEL lv_hex.set(fs.readFileSync("./node_modules/scrypt-wasm/scrypt_wasm_bg.wasm").toString("hex").toUpperCase());'.

    GET RUN TIME FIELD DATA(lv_start).
    DATA(li_wasm) = zcl_wasm=>create_with_wasm( lv_hex ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_runtime) = lv_end - lv_start.
    WRITE / |{ lv_runtime }ms parsing Scrypt-WASM|.

    rv_json = '{"runtime": "' && lv_runtime && '"}'.

**********************************************************************

    CONSTANTS lc_password TYPE xstring VALUE 'AABBCCAABBCC'.
    CONSTANTS lc_salt     TYPE xstring VALUE 'AABBCC'.

    DATA(lt_results) = li_wasm->execute_function_export(
      iv_name       = '__wbindgen_malloc'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( xstrlen( lc_password ) ) ) ) ).
    DATA(lo_password_ptr) = CAST zcl_wasm_i32( lt_results[ 1 ] ).
    " WRITE: / 'password ptr offset:', lo_password_ptr->get_signed( ).

    CLEAR lt_results.
    lt_results = li_wasm->execute_function_export(
      iv_name       = '__wbindgen_malloc'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( xstrlen( lc_salt ) ) ) ) ).
    DATA(lo_salt_ptr) = CAST zcl_wasm_i32( lt_results[ 1 ] ).
    " WRITE: / 'salt ptr offset:', lo_salt_ptr->get_signed( ).

    CLEAR lt_results.
    lt_results = li_wasm->execute_function_export( '__wbindgen_global_argument_ptr' ).
    DATA(lo_retptr) = lt_results[ 1 ].
    " WRITE: / 'gbl ptr:', lo_salt_ptr->get_signed( ).

* "scrypt" function export takes 9 x i32
    li_wasm->execute_function_export(
      iv_name       = 'scrypt'
      it_parameters = VALUE #(
        ( lo_retptr )
        ( lo_password_ptr )
        ( zcl_wasm_i32=>from_signed( xstrlen( lc_password ) ) )
        ( lo_salt_ptr )
        ( zcl_wasm_i32=>from_signed( xstrlen( lc_salt ) ) )
        ( zcl_wasm_i32=>from_signed( 1 ) )
        ( zcl_wasm_i32=>from_signed( 1 ) )
        ( zcl_wasm_i32=>from_signed( 1 ) )
        ( zcl_wasm_i32=>from_signed( 1 ) ) ) ).

  ENDMETHOD.

ENDCLASS.
