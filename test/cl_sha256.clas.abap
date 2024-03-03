CLASS cl_sha256 DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING VALUE(rv_json) TYPE string
      RAISING zcx_wasm.
ENDCLASS.

CLASS cl_sha256 IMPLEMENTATION.

  METHOD run.

    DATA lv_hex TYPE xstring.

    WRITE '@KERNEL const fs = await import("fs");'.
    WRITE '@KERNEL lv_hex.set(fs.readFileSync("./test/sha256.wasm").toString("hex").toUpperCase());'.

    GET RUN TIME FIELD DATA(lv_start).
    DATA(li_wasm) = zcl_wasm=>create_with_wasm( lv_hex ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_runtime) = lv_end - lv_start.
    WRITE / |{ lv_runtime }ms parsing SHA256|.

    rv_json = '{"runtime": "' && lv_runtime && '"}'.

    " todo, execute instruction zcl_wasm_i32_wrap_i64'

    " takes 4 x i32 as input
    " DATA(lt_results) = li_wasm->execute_function_export(
    "   iv_name       = 'sha256'
    "   it_parameters = VALUE #(
    "     ( zcl_wasm_i32=>from_signed( 1 ) )
    "     ( zcl_wasm_i32=>from_signed( 1 ) )
    "     ( zcl_wasm_i32=>from_signed( 1 ) )
    "     ( zcl_wasm_i32=>from_signed( 1 ) )
    "   ) ).

  ENDMETHOD.

ENDCLASS.
