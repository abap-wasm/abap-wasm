CLASS cl_quickjs DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_hex         TYPE xstring
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_wasm.
  PRIVATE SECTION.
    CLASS-DATA gi_wasm TYPE REF TO zif_wasm_module.

    CLASS-METHODS string_to_vm
      IMPORTING
        iv_string TYPE string
      RETURNING
        VALUE(ro_pointer) TYPE REF TO zcl_wasm_i32
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS cl_quickjs IMPLEMENTATION.

  METHOD run.

* https://emscripten.org/docs/compiling/WebAssembly.html
* https://www.npmjs.com/package/@jitl/quickjs-wasmfile-release-sync
* https://www.npmjs.com/package/@jitl/quickjs-wasmfile-debug-sync

    DATA(lo_env) = CAST zif_wasm_module( NEW cl_quickjs_env( ) )->instantiate( ).
    DATA(lo_wasi) = CAST zif_wasm_module( NEW cl_quickjs_wasi_preview( lo_env->get_memory( ) ) )->instantiate( ).

    GET RUN TIME FIELD DATA(lv_start).
    gi_wasm = zcl_wasm=>create_with_wasm(
      iv_wasm    = iv_hex
      it_imports = VALUE #(
        ( name = 'env'                    module = lo_env )
        ( name = 'wasi_snapshot_preview1' module = lo_wasi ) ) ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_runtime) = lv_end - lv_start.
    WRITE / |{ lv_runtime }ms parsing QuickJS|.

    rv_json = '{"runtime": "' && lv_runtime && '"}'.

* https://github.com/justjake/quickjs-emscripten/blob/main/c/interface.c

* JSRuntime *QTS_NewRuntime() {
* (result i32)
    DATA(lt_result) = gi_wasm->execute_function_export( 'QTS_NewRuntime' ).
    DATA(lv_runtime_ptr) = lt_result[ 1 ].

* IntrinsicsFlags = https://github.com/justjake/quickjs-emscripten/blob/cc9b624930dfb319a0198587386c405b86af4740/packages/quickjs-emscripten-core/src/types.ts#L145
* JSContext *QTS_NewContext(JSRuntime *rt, IntrinsicsFlags intrinsics) {
* (param i32 i32) (result i32)
    lt_result = gi_wasm->execute_function_export(
      iv_name       = 'QTS_NewContext'
      it_parameters = VALUE #(
        ( lv_runtime_ptr )
        ( zcl_wasm_i32=>from_signed( 0 ) ) ) ).
    DATA(lv_context_ptr) = lt_result[ 1 ].

* MaybeAsync(JSValue *) QTS_Eval(JSContext *ctx, BorrowedHeapChar *js_code, size_t js_code_length, const char *filename, EvalDetectModule detectModule, EvalFlags evalFlags) {
* EvalDetectModule is 0 or 1
* EvalFlags see https://github.com/justjake/quickjs-emscripten/blob/cc9b624930dfb319a0198587386c405b86af4740/packages/quickjs-emscripten-core/src/types.ts#L266
* (param i32 i32 i32 i32 i32 i32) (result i32)
    DATA(lv_code) = |1 + 5|.
    DATA(lo_code_ptr) = string_to_vm( lv_code ).
    DATA(lo_filename_ptr) = string_to_vm( |test.js| ).
    gi_wasm->execute_function_export(
      iv_name       = 'QTS_Eval'
      it_parameters = VALUE #(
        ( lv_context_ptr )
        ( lo_code_ptr )
        ( zcl_wasm_i32=>from_signed( strlen( lv_code ) ) )
        ( lo_filename_ptr )
        ( zcl_wasm_i32=>from_signed( 0 ) )
        ( zcl_wasm_i32=>from_signed( 0 ) ) ) ).

  ENDMETHOD.

  METHOD string_to_vm.

    CONSTANTS lc_null TYPE x LENGTH 1 VALUE '00'.
    DATA lv_xstr TYPE xstring.

    DATA(lv_length) = strlen( iv_string ) + 1.

    DATA(lt_result) = gi_wasm->execute_function_export(
      iv_name       = 'malloc'
      it_parameters = VALUE #(
        ( zcl_wasm_i32=>from_signed( lv_length ) ) ) ).

    ro_pointer = CAST zcl_wasm_i32( lt_result[ 1 ] ).

    lv_xstr = cl_abap_codepage=>convert_to( iv_string ).
    CONCATENATE lv_xstr lc_null INTO lv_xstr IN BYTE MODE.

    gi_wasm->get_memory( )->get_linear( )->set(
      iv_offset = ro_pointer->get_signed( )
      iv_bytes  = lv_xstr ).

  ENDMETHOD.

ENDCLASS.
