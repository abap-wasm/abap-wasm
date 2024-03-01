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

  ENDMETHOD.

ENDCLASS.
