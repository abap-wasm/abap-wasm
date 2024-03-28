CLASS cl_sha256 DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING VALUE(rv_json) TYPE string
      RAISING zcx_wasm.
ENDCLASS.

CLASS cl_sha256 IMPLEMENTATION.

  METHOD run.

    CONSTANTS lc_statebytes   TYPE i VALUE 108.
    CONSTANTS lc_input_offset TYPE int8 VALUE 40.
    CONSTANTS lc_block_size   TYPE i VALUE 64.
    CONSTANTS lc_sha256_bytes TYPE int8 VALUE 32.

    DATA lv_hex   TYPE xstring.
    DATA lv_head  TYPE i.
    DATA lv_input TYPE string.


    WRITE '@KERNEL const fs = await import("fs");'.
    WRITE '@KERNEL lv_hex.set(fs.readFileSync("./test/sha256.wasm").toString("hex").toUpperCase());'.

    GET RUN TIME FIELD DATA(lv_start).
    DATA(li_wasm) = zcl_wasm=>create_with_wasm( lv_hex ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_runtime) = lv_end - lv_start.
    WRITE / |{ lv_runtime }ms parsing SHA256|.

    rv_json = '{"runtime": "' && lv_runtime && '"}'.

****************************************************************************************************

    lv_head = lv_head + lc_statebytes.
    lv_input = 'hello world'.

    li_wasm->instantiate( ).

    DATA(li_linear) = li_wasm->get_memory( )->mi_linear.
    li_linear->set(
      iv_bytes  = cl_abap_codepage=>convert_to( lv_input )
      iv_offset = lc_input_offset ).

    " update()
    li_wasm->execute_function_export(
      iv_name       = 'sha256'
      it_parameters = VALUE #(
        ( zcl_wasm_i32=>from_signed( 0 ) )
        ( zcl_wasm_i32=>from_signed( lv_head ) )
        ( zcl_wasm_i32=>from_signed( strlen( lv_input ) ) )
        ( zcl_wasm_i32=>from_signed( 0 ) )
      ) ).

    " digest()
    li_wasm->execute_function_export(
      iv_name       = 'sha256'
      it_parameters = VALUE #(
        ( zcl_wasm_i32=>from_signed( 0 ) )
        ( zcl_wasm_i32=>from_signed( lv_head ) )
        ( zcl_wasm_i32=>from_signed( 0 ) )
        ( zcl_wasm_i32=>from_signed( 1 ) )
      ) ).

    DATA(lv_result) = li_linear->get(
      iv_length = lc_sha256_bytes
      iv_offset = 0 ).

    DATA(lv_str) = to_lower( |{ zcl_wasm_binary_stream=>reverse_hex( lv_result ) }| ).
    WRITE / lv_str.
    ASSERT lv_str = |b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9|.

  ENDMETHOD.

ENDCLASS.
