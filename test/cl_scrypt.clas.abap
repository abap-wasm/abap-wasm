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

    " DATA(lt_results) = li_wasm->execute_function_export( '__wbindgen_global_argument_ptr' ).
    " DATA(lv_retptr) = lt_results[ 1 ].

    " todo, 5 should be the encoded length of the password
    " malloc is function 38
    TRY.
        DATA(lt_results) = li_wasm->execute_function_export(
          iv_name       = '__wbindgen_malloc'
          it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( 6 ) ) ) ).
      CATCH zcx_wasm INTO DATA(lx_wasm).
        WRITE / li_wasm->dump_stack( ).
        WRITE / li_wasm->dump_linear_memory( ).

        " DATA(lv_mem) = li_wasm->get_memory( )->get_linear( )->get(
        "   iv_length = 22
        "   iv_offset = 1050832 ).
        " WRITE / lv_mem.
    ENDTRY.

* "scrypt" function export takes 9 x i32
    " li_wasm->execute_function_export(
    "   iv_name       = 'scrypt'
    "   it_parameters = VALUE #(
    "     ( lv_retptr )
    "     ( password_ptr )
    "     ( password_len )
    "     ( salt_ptr )
    "     ( salt_len )
    "     ( todo )
    "     ( todo )
    "     ( todo )
    "     ( todo ) ) ).

  ENDMETHOD.

ENDCLASS.
