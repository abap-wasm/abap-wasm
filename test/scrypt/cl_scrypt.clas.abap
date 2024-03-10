CLASS cl_scrypt DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS cl_scrypt IMPLEMENTATION.

  METHOD run.

    DATA lv_hex TYPE xstring.

    WRITE '@KERNEL const fs = await import("fs");'.
    WRITE '@KERNEL lv_hex.set(fs.readFileSync("./node_modules/scrypt-rs-wasm/scrypt_rs_wasm_bg.wasm").toString("hex").toUpperCase());'.

    GET RUN TIME FIELD DATA(lv_start).
    DATA(li_wasm) = zcl_wasm=>create_with_wasm(
      iv_wasm    = lv_hex
      it_imports = VALUE #( (
        name   = '__wbindgen_placeholder__'
        module = NEW cl_wbindgen_placeholder( ) ) ) ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_runtime) = lv_end - lv_start.
    WRITE / |{ lv_runtime }ms parsing Scrypt-WASM|.

    rv_json = '{"runtime": "' && lv_runtime && '"}'.

****************************************
* https://rustwasm.github.io/docs/book/reference/debugging.html

    DATA(lt_results) = li_wasm->execute_function_export(
      iv_name       = '__wbindgen_add_to_stack_pointer'
      it_parameters = VALUE #( ( zcl_wasm_i32=>from_signed( -16 ) ) ) ).
    DATA(lo_retptr) = CAST zcl_wasm_i32( lt_results[ 1 ] ).

"     TRY.
"         lt_results = li_wasm->execute_function_export(
"           iv_name       = 'run'
"           it_parameters = VALUE #( ( lo_retptr ) ) ).
"         LOOP AT lt_results INTO DATA(li_result).
"           WRITE / |{ li_result->get_type( ) }: { li_result->human_readable_value( ) }|.
"         ENDLOOP.

"         DATA(li_linear) = li_wasm->get_memory( )->get_linear( ).
"         DATA(lv_realptr) = li_linear->get(
"           iv_length = 4
"           iv_offset = lo_retptr->get_signed( ) ).
"         " WRITE / lv_realptr.

"         DATA(lv_reallen) = li_linear->get(
"           iv_length = 4
"           iv_offset = lo_retptr->get_signed( ) + 4 ).
"         " WRITE / lv_reallen.

"         DATA(lv_realret) = li_linear->get_raw(
"           iv_length = CONV #( lv_reallen )
"           iv_offset = CONV #( lv_realptr ) ).
" * expected value = "Hello 636d8985f1148f8a10f9f925f4e3e895b867bdf43a8f796fc8c49926406519fae4a29b2e492f76ce3b0bd96143264b04ee86decf16f9c1396d4de96ea453b8a2"
"         WRITE / cl_abap_codepage=>convert_from( lv_realret ).
"       CATCH zcx_wasm INTO DATA(lo_exception).
"         WRITE / |Exception: { lo_exception->get_text( ) } |.
"     ENDTRY.

* running optimized version:
" Error: block
"     at zcl_wasm_block_helper.end (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_block_helper.clas.mjs:89:13)
"     at async zcl_wasm_block.zif_wasm_instruction$execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_block.clas.mjs:71:9)
"     at async zcl_wasm_vm.execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_vm.clas.mjs:55:23)
"     at async zcl_wasm_block.zif_wasm_instruction$execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_block.clas.mjs:66:23)
"     at async zcl_wasm_vm.execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_vm.clas.mjs:55:23)
"     at async zcl_wasm_call.invoke (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_call.clas.mjs:103:7)
"     at async zcl_wasm_call_indirect.zif_wasm_instruction$execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_call_indirect.clas.mjs:86:5)
"     at async zcl_wasm_vm.execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_vm.clas.mjs:55:23)
"     at async zcl_wasm_loop.zif_wasm_instruction$execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_loop.clas.mjs:72:25)
"     at async zcl_wasm_vm.execute (file:///C:/Users/Lars/git/abap-wasm/output/zcl_wasm_vm.clas.mjs:55:23)

  ENDMETHOD.

ENDCLASS.
