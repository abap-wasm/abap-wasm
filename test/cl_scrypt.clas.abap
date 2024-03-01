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

    DATA lv_password TYPE string VALUE 'password'.
    DATA lv_salt TYPE string VALUE 'salt'.

    DATA(lt_results) = li_wasm->execute_function_export( '__wbindgen_global_argument_ptr' ).
    DATA(lv_retptr) = lt_results[ 1 ].

    " todo, 5 should be the encoded length of the password
    " lt_results = li_wasm->execute_function_export(
    "   iv_name       = '__wbindgen_malloc'
    "   it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( 5 ) ) ) ).

* "scrypt" function export takes 9 x i32
    " li_wasm->execute_function_export(
    "   iv_name       = 'scrypt'
    "   it_parameters = VALUE #(
    "     ( lv_retptr )
    "     ( todo )
    "     ( todo )
    "     ( todo )
    "     ( todo )
    "     ( todo )
    "     ( todo )
    "     ( todo )
    "     ( todo ) ) ).

  ENDMETHOD.

ENDCLASS.
