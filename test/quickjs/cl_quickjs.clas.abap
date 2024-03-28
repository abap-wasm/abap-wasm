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
    CONSTANTS gc_null TYPE x LENGTH 1 VALUE '00'.
    CLASS-DATA gi_wasm TYPE REF TO zif_wasm_module.

    CLASS-METHODS string_to_vm
      IMPORTING
        iv_string         TYPE string
      RETURNING
        VALUE(ro_pointer) TYPE REF TO zcl_wasm_i32
      RAISING
        zcx_wasm.

    CLASS-METHODS string_from_vm
      IMPORTING
        io_pointer       TYPE REF TO zcl_wasm_i32
      RETURNING
        VALUE(rv_string) TYPE string
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

    DATA(lv_parsing) = lv_end - lv_start.
    WRITE / |{ lv_parsing }ms parsing QuickJS|.

* https://github.com/justjake/quickjs-emscripten/blob/main/c/interface.c

    GET RUN TIME FIELD lv_start.


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
        ( zcl_wasm_i32=>from_signed( 1023 ) ) ) ).
    DATA(lv_context_ptr) = lt_result[ 1 ].

* MaybeAsync(JSValue *) QTS_Eval(JSContext *ctx, BorrowedHeapChar *js_code, size_t js_code_length, const char *filename, EvalDetectModule detectModule, EvalFlags evalFlags) {
* EvalDetectModule is 0 or 1
* EvalFlags see https://github.com/justjake/quickjs-emscripten/blob/cc9b624930dfb319a0198587386c405b86af4740/packages/quickjs-emscripten-core/src/types.ts#L266
* (param i32 i32 i32 i32 i32 i32) (result i32)
    " DATA lv_base64 TYPE string.
    " lv_base64 = lv_base64 && |Lyplc2xpbnQtZGlzYWJsZSovDQoNCi8vIC4uL2NvcmUvc3JjL2ZpbGVzL19hYnN0cmFjdF9maWxlLnRzDQp2YXIgQWJzdHJhY3RGaWxlID0gY2xhc3Mgew0KICBjb25z|.
    " lv_base64 = lv_base64 && |dHJ1Y3RvcihmaWxlbmFtZSkgew0KICAgIHRoaXMuZmlsZW5hbWUgPSBmaWxlbmFtZTsNCiAgfQ0KICBnZXRGaWxlbmFtZSgpIHsNCiAgICByZXR1cm4gdGhpcy5maWxl|.
    " lv_base64 = lv_base64 && |bmFtZTsNCiAgfQ0KICBiYXNlTmFtZSgpIHsNCiAgICBsZXQgbmFtZSA9IHRoaXMuZ2V0RmlsZW5hbWUoKTsNCiAgICBsZXQgaW5kZXggPSBuYW1lLmxhc3RJbmRleE9m|.
    " lv_base64 = lv_base64 && |KCJcXCIpOw0KICAgIGlmIChpbmRleCkgew0KICAgICAgaW5kZXggPSBpbmRleCArIDE7DQogICAgfQ0KICAgIG5hbWUgPSBuYW1lLnN1YnN0cmluZyhpbmRleCk7DQog|.
    " lv_base64 = lv_base64 && |ICAgaW5kZXggPSBuYW1lLmxhc3RJbmRleE9mKCIvIik7DQogICAgaWYgKGluZGV4KSB7DQogICAgICBpbmRleCA9IGluZGV4ICsgMTsNCiAgICB9DQogICAgcmV0dXJu|.
    " lv_base64 = lv_base64 && |IG5hbWUuc3Vic3RyaW5nKGluZGV4KTsNCiAgfQ0KICBnZXRPYmplY3RUeXBlKCkgew0KICAgIGNvbnN0IHNwbGl0ID0gdGhpcy5iYXNlTmFtZSgpLnNwbGl0KCIuIik7|.
    " lv_base64 = lv_base64 && |DQogICAgcmV0dXJuIHNwbGl0WzFdPy50b1VwcGVyQ2FzZSgpOw0KICB9DQogIGdldE9iamVjdE5hbWUoKSB7DQogICAgY29uc3Qgc3BsaXQgPSB0aGlzLmJhc2VOYW1l|.
    " lv_base64 = lv_base64 && |KCkuc3BsaXQoIi4iKTsNCiAgICBzcGxpdFswXSA9IHNwbGl0WzBdLnJlcGxhY2UoLyUyMy9nLCAiIyIpOw0KICAgIHNwbGl0WzBdID0gc3BsaXRbMF0ucmVwbGFjZSgv|.
    " lv_base64 = lv_base64 && |JTNlL2csICI+Iik7DQogICAgc3BsaXRbMF0gPSBzcGxpdFswXS5yZXBsYWNlKC8lM2MvZywgIjwiKTsNCiAgICBzcGxpdFswXSA9IHNwbGl0WzBdLnRvVXBwZXJDYXNl|.
    " lv_base64 = lv_base64 && |KCkucmVwbGFjZSgvIy9nLCAiLyIpOw0KICAgIHNwbGl0WzBdID0gc3BsaXRbMF0ucmVwbGFjZSgiKCIsICIvIik7DQogICAgc3BsaXRbMF0gPSBzcGxpdFswXS5yZXBs|.
    " lv_base64 = lv_base64 && |YWNlKCIpIiwgIi8iKTsNCiAgICByZXR1cm4gc3BsaXRbMF07DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2ZpbGVzL21lbW9yeV9maWxlLnRzDQp2YXIgTWVtb3J5|.
    " lv_base64 = lv_base64 && |RmlsZSA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RGaWxlIHsNCiAgY29uc3RydWN0b3IoZmlsZW5hbWUsIHJhdykgew0KICAgIHN1cGVyKGZpbGVuYW1lKTsNCiAgICB0|.
    " lv_base64 = lv_base64 && |aGlzLnJhdyA9IHJhdzsNCiAgfQ0KICBnZXRSYXcoKSB7DQogICAgcmV0dXJuIHRoaXMucmF3Ow0KICB9DQogIGdldFJhd1Jvd3MoKSB7DQogICAgcmV0dXJuIHRoaXMu|.
    " lv_base64 = lv_base64 && |cmF3LnNwbGl0KCJcbiIpOw0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9wb3NpdGlvbi50cw0KdmFyIFBvc2l0aW9uID0gY2xhc3Mgew0KICBjb25zdHJ1Y3Rvcihy|.
    " lv_base64 = lv_base64 && |b3csIGNvbCkgew0KICAgIHRoaXMucm93ID0gcm93Ow0KICAgIHRoaXMuY29sID0gY29sOw0KICB9DQogIGdldENvbCgpIHsNCiAgICByZXR1cm4gdGhpcy5jb2w7DQog|.
    " lv_base64 = lv_base64 && |IH0NCiAgZ2V0Um93KCkgew0KICAgIHJldHVybiB0aGlzLnJvdzsNCiAgfQ0KICBpc0FmdGVyKHApIHsNCiAgICByZXR1cm4gdGhpcy5yb3cgPiBwLnJvdyB8fCB0aGlz|.
    " lv_base64 = lv_base64 && |LnJvdyA9PT0gcC5yb3cgJiYgdGhpcy5jb2wgPj0gcC5jb2w7DQogIH0NCiAgZXF1YWxzKHApIHsNCiAgICByZXR1cm4gdGhpcy5yb3cgPT09IHAuZ2V0Um93KCkgJiYg|.
    " lv_base64 = lv_base64 && |dGhpcy5jb2wgPT09IHAuZ2V0Q29sKCk7DQogIH0NCiAgaXNCZWZvcmUocCkgew0KICAgIHJldHVybiB0aGlzLnJvdyA8IHAucm93IHx8IHRoaXMucm93ID09PSBwLnJv|.
    " lv_base64 = lv_base64 && |dyAmJiB0aGlzLmNvbCA8IHAuY29sOw0KICB9DQogIGlzQmV0d2VlbihwMSwgcDIpIHsNCiAgICByZXR1cm4gdGhpcy5pc0FmdGVyKHAxKSAmJiB0aGlzLmlzQmVmb3Jl|.
    " lv_base64 = lv_base64 && |KHAyKTsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvdmlydHVhbF9wb3NpdGlvbi50cw0KdmFyIFZpcnR1YWxQb3NpdGlvbiA9IGNsYXNzIF9WaXJ0dWFsUG9zaXRp|.
    " lv_base64 = lv_base64 && |b24gZXh0ZW5kcyBQb3NpdGlvbiB7DQogIGNvbnN0cnVjdG9yKHZpcnR1YWwsIHJvdywgY29sKSB7DQogICAgc3VwZXIodmlydHVhbC5nZXRSb3coKSwgdmlydHVhbC5n|.
    " lv_base64 = lv_base64 && |ZXRDb2woKSk7DQogICAgdGhpcy52cm93ID0gcm93Ow0KICAgIHRoaXMudmNvbCA9IGNvbDsNCiAgfQ0KICBlcXVhbHMocCkgew0KICAgIGlmICghKHAgaW5zdGFuY2Vv|.
    " lv_base64 = lv_base64 && |ZiBfVmlydHVhbFBvc2l0aW9uKSkgew0KICAgICAgcmV0dXJuIGZhbHNlOw0KICAgIH0NCiAgICBjb25zdCBjYXN0ZWQgPSBwOw0KICAgIHJldHVybiBzdXBlci5lcXVh|.
    " lv_base64 = lv_base64 && |bHModGhpcykgJiYgdGhpcy52cm93ID09PSBjYXN0ZWQudnJvdyAmJiB0aGlzLnZjb2wgPT09IGNhc3RlZC52Y29sOw0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9h|.
    " lv_base64 = lv_base64 && |YmFwLzFfbGV4ZXIvdG9rZW5zL2Fic3RyYWN0X3Rva2VuLnRzDQp2YXIgQWJzdHJhY3RUb2tlbiA9IGNsYXNzIHsNCiAgY29uc3RydWN0b3Ioc3RhcnQsIHN0cikgew0K|.
    " lv_base64 = lv_base64 && |ICAgIHRoaXMuc3RhcnQgPSBzdGFydDsNCiAgICB0aGlzLnN0ciA9IHN0cjsNCiAgfQ0KICAvLyBzcGVjaWFsIGZ1bmN0aW9uLCBmb3IgZGVidWdnaW5nIHB1cnBvc2Vz|.
    " lv_base64 = lv_base64 && |LCBzZWUgaHR0cHM6Ly9naXRodWIuY29tL2FiYXBsaW50L2FiYXBsaW50L3B1bGwvMzEzNw0KICBbU3ltYm9sLmZvcigiZGVidWcuZGVzY3JpcHRpb24iKV0oKSB7DQog|.
    " lv_base64 = lv_base64 && |ICAgcmV0dXJuIGAke3RoaXMuY29uc3RydWN0b3IubmFtZX0gJHt0aGlzLnN0cn1gOw0KICB9DQogIGdldFN0cigpIHsNCiAgICByZXR1cm4gdGhpcy5zdHI7DQogIH0N|.
    " lv_base64 = lv_base64 && |CiAgZ2V0Um93KCkgew0KICAgIHJldHVybiB0aGlzLnN0YXJ0LmdldFJvdygpOw0KICB9DQogIGdldENvbCgpIHsNCiAgICByZXR1cm4gdGhpcy5zdGFydC5nZXRDb2wo|.
    " lv_base64 = lv_base64 && |KTsNCiAgfQ0KICBnZXRTdGFydCgpIHsNCiAgICByZXR1cm4gdGhpcy5zdGFydDsNCiAgfQ0KICBnZXRFbmQoKSB7DQogICAgcmV0dXJuIG5ldyBQb3NpdGlvbih0aGlz|.
    " lv_base64 = lv_base64 && |LnN0YXJ0LmdldFJvdygpLCB0aGlzLnN0YXJ0LmdldENvbCgpICsgdGhpcy5zdHIubGVuZ3RoKTsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVy|.
    " lv_base64 = lv_base64 && |L3Rva2Vucy9hdC50cw0KdmFyIEF0ID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiQCI7DQog|.
    " lv_base64 = lv_base64 && |IH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvYXR3LnRzDQp2YXIgQXRXID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAg|.
    " lv_base64 = lv_base64 && |c3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiQCAiOw0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3dhdC50cw0KdmFy|.
    " lv_base64 = lv_base64 && |IFdBdCA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIiBAIjsNCiAgfQ0KfTsNCg0KLy8gLi4v|.
    " lv_base64 = lv_base64 && |Y29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy93YXR3LnRzDQp2YXIgV0F0VyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9h|.
    " lv_base64 = lv_base64 && |ZCgpIHsNCiAgICByZXR1cm4gIiBAICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvYnJhY2tldF9sZWZ0LnRzDQp2YXIgQnJh|.
    " lv_base64 = lv_base64 && |Y2tldExlZnQgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICJbIjsNCiAgfQ0KfTsNCg0KLy8g|.
    " lv_base64 = lv_base64 && |Li4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy93YnJhY2tldF9sZWZ0LnRzDQp2YXIgV0JyYWNrZXRMZWZ0ID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2Vu|.
    " lv_base64 = lv_base64 && |IHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiIFsiOw0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL2JyYWNr|.
    " lv_base64 = lv_base64 && |ZXRfbGVmdHcudHMNCnZhciBCcmFja2V0TGVmdFcgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJu|.
    " lv_base64 = lv_base64 && |ICJbICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvd2JyYWNrZXRfbGVmdHcudHMNCnZhciBXQnJhY2tldExlZnRXID0gY2xh|.
    " lv_base64 = lv_base64 && |c3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiIFsgIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMv|.
    " lv_base64 = lv_base64 && |YWJhcC8xX2xleGVyL3Rva2Vucy9icmFja2V0X3JpZ2h0LnRzDQp2YXIgQnJhY2tldFJpZ2h0ID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGlj|.
    " lv_base64 = lv_base64 && |IHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiXSI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvd2JyYWNrZXRfcmlnaHQudHMN|.
    " lv_base64 = lv_base64 && |CnZhciBXQnJhY2tldFJpZ2h0ID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiIF0iOw0KICB9|.
    " lv_base64 = lv_base64 && |DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL2JyYWNrZXRfcmlnaHR3LnRzDQp2YXIgQnJhY2tldFJpZ2h0VyA9IGNsYXNzIGV4dGVuZHMg|.
    " lv_base64 = lv_base64 && |QWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIl0gIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVy|.
    " lv_base64 = lv_base64 && |L3Rva2Vucy93YnJhY2tldF9yaWdodHcudHMNCnZhciBXQnJhY2tldFJpZ2h0VyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9h|.
    " lv_base64 = lv_base64 && |ZCgpIHsNCiAgICByZXR1cm4gIiBdICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvaW5zdGFuY2VfYXJyb3cudHMNCnZhciBJ|.
    " lv_base64 = lv_base64 && |bnN0YW5jZUFycm93ID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiLT4iOw0KICB9DQp9Ow0K|.
    " lv_base64 = lv_base64 && |DQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3dpbnN0YW5jZV9hcnJvdy50cw0KdmFyIFdJbnN0YW5jZUFycm93ID0gY2xhc3MgZXh0ZW5kcyBBYnN0|.
    " lv_base64 = lv_base64 && |cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiIC0+IjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rv|.
    " lv_base64 = lv_base64 && |a2Vucy9pbnN0YW5jZV9hcnJvd3cudHMNCnZhciBJbnN0YW5jZUFycm93VyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgp|.
    " lv_base64 = lv_base64 && |IHsNCiAgICByZXR1cm4gIi0+ICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvd2luc3RhbmNlX2Fycm93dy50cw0KdmFyIFdJ|.
    " lv_base64 = lv_base64 && |bnN0YW5jZUFycm93VyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIiAtPiAiOw0KICB9DQp9|.
    " lv_base64 = lv_base64 && |Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3BhcmVuX2xlZnQudHMNCnZhciBQYXJlbkxlZnQgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9r|.
    " lv_base64 = lv_base64 && |ZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICIoIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy93cGFy|.
    " lv_base64 = lv_base64 && |ZW5fbGVmdC50cw0KdmFyIFdQYXJlbkxlZnQgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICIg|.
    " lv_base64 = lv_base64 && |KCI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvcGFyZW5fbGVmdHcudHMNCnZhciBQYXJlbkxlZnRXID0gY2xhc3MgZXh0ZW5k|.
    " lv_base64 = lv_base64 && |cyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiKCAiOw0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4|.
    " lv_base64 = lv_base64 && |ZXIvdG9rZW5zL3dwYXJlbl9sZWZ0dy50cw0KdmFyIFdQYXJlbkxlZnRXID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkg|.
    " lv_base64 = lv_base64 && |ew0KICAgIHJldHVybiAiICggIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy9wYXJlbl9yaWdodC50cw0KdmFyIFBhcmVuUmln|.
    " lv_base64 = lv_base64 && |aHQgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICIpIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29y|.
    " lv_base64 = lv_base64 && |ZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy93cGFyZW5fcmlnaHQudHMNCnZhciBXUGFyZW5SaWdodCA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0|.
    " lv_base64 = lv_base64 && |YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIiApIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy9wYXJlbl9yaWdodHcu|.
    " lv_base64 = lv_base64 && |dHMNCnZhciBQYXJlblJpZ2h0VyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIikgIjsNCiAg|.
    " lv_base64 = lv_base64 && |fQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy93cGFyZW5fcmlnaHR3LnRzDQp2YXIgV1BhcmVuUmlnaHRXID0gY2xhc3MgZXh0ZW5kcyBB|.
    " lv_base64 = lv_base64 && |YnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiICkgIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVy|.
    " lv_base64 = lv_base64 && |L3Rva2Vucy9kYXNoLnRzDQp2YXIgRGFzaCA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIi0i|.
    " lv_base64 = lv_base64 && |Ow0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3dkYXNoLnRzDQp2YXIgV0Rhc2ggPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9r|.
    " lv_base64 = lv_base64 && |ZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICIgLSI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvZGFz|.
    " lv_base64 = lv_base64 && |aHcudHMNCnZhciBEYXNoVyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIi0gIjsNCiAgfQ0K|.
    " lv_base64 = lv_base64 && |fTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy93ZGFzaHcudHMNCnZhciBXRGFzaFcgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0K|.
    " lv_base64 = lv_base64 && |ICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICIgLSAiOw0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3BsdXMudHMN|.
    " lv_base64 = lv_base64 && |CnZhciBQbHVzID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiKyI7DQogIH0NCn07DQoNCi8v|.
    " lv_base64 = lv_base64 && |IC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvd3BsdXMudHMNCnZhciBXUGx1cyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyBy|.
    " lv_base64 = lv_base64 && |YWlscm9hZCgpIHsNCiAgICByZXR1cm4gIiArIjsNCiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy9wbHVzdy50cw0KdmFyIFBsdXNX|.
    " lv_base64 = lv_base64 && |ID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiKyAiOw0KICB9DQp9Ow0KDQovLyAuLi9jb3Jl|.
    " lv_base64 = lv_base64 && |L3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3dwbHVzdy50cw0KdmFyIFdQbHVzVyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9h|.
    " lv_base64 = lv_base64 && |ZCgpIHsNCiAgICByZXR1cm4gIiArICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvc3RhdGljX2Fycm93LnRzDQp2YXIgU3Rh|.
    " lv_base64 = lv_base64 && |dGljQXJyb3cgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KICBzdGF0aWMgcmFpbHJvYWQoKSB7DQogICAgcmV0dXJuICI9PiI7DQogIH0NCn07DQoNCi8v|.
    " lv_base64 = lv_base64 && |IC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvd3N0YXRpY19hcnJvdy50cw0KdmFyIFdTdGF0aWNBcnJvdyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tl|.
    " lv_base64 = lv_base64 && |biB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1cm4gIiA9PiI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvc3Rh|.
    " lv_base64 = lv_base64 && |dGljX2Fycm93dy50cw0KdmFyIFN0YXRpY0Fycm93VyA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQogIHN0YXRpYyByYWlscm9hZCgpIHsNCiAgICByZXR1|.
    " lv_base64 = lv_base64 && |cm4gIj0+ICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvd3N0YXRpY19hcnJvd3cudHMNCnZhciBXU3RhdGljQXJyb3dXID0g|.
    " lv_base64 = lv_base64 && |Y2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsNCiAgc3RhdGljIHJhaWxyb2FkKCkgew0KICAgIHJldHVybiAiID0+ICI7DQogIH0NCn07DQoNCi8vIC4uL2NvcmUv|.
    " lv_base64 = lv_base64 && |c3JjL2FiYXAvMV9sZXhlci90b2tlbnMvc3RyaW5nLnRzDQp2YXIgU3RyaW5nVG9rZW4gPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KfTsNCg0KLy8gLi4v|.
    " lv_base64 = lv_base64 && |Y29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy9zdHJpbmdfdGVtcGxhdGUudHMNCnZhciBTdHJpbmdUZW1wbGF0ZSA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tl|.
    " lv_base64 = lv_base64 && |biB7DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL3N0cmluZ190ZW1wbGF0ZV9iZWdpbi50cw0KdmFyIFN0cmluZ1RlbXBsYXRlQmVnaW4g|.
    " lv_base64 = lv_base64 && |PSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy9zdHJpbmdfdGVtcGxhdGVfZW5kLnRz|.
    " lv_base64 = lv_base64 && |DQp2YXIgU3RyaW5nVGVtcGxhdGVFbmQgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vu|.
    " lv_base64 = lv_base64 && |cy9zdHJpbmdfdGVtcGxhdGVfbWlkZGxlLnRzDQp2YXIgU3RyaW5nVGVtcGxhdGVNaWRkbGUgPSBjbGFzcyBleHRlbmRzIEFic3RyYWN0VG9rZW4gew0KfTsNCg0KLy8g|.
    " lv_base64 = lv_base64 && |Li4vY29yZS9zcmMvYWJhcC8xX2xleGVyL3Rva2Vucy9jb21tZW50LnRzDQp2YXIgQ29tbWVudCA9IGNsYXNzIGV4dGVuZHMgQWJzdHJhY3RUb2tlbiB7DQp9Ow0KDQov|.
    " lv_base64 = lv_base64 && |LyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvdG9rZW5zL2lkZW50aWZpZXIudHMNCnZhciBJZGVudGlmaWVyID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsN|.
    " lv_base64 = lv_base64 && |Cn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvcHJhZ21hLnRzDQp2YXIgUHJhZ21hID0gY2xhc3MgZXh0ZW5kcyBBYnN0cmFjdFRva2VuIHsN|.
    " lv_base64 = lv_base64 && |Cn07DQoNCi8vIC4uL2NvcmUvc3JjL2FiYXAvMV9sZXhlci90b2tlbnMvcHVuY3R1YXRpb24udHMNCnZhciBQdW5jdHVhdGlvbiA9IGNsYXNzIGV4dGVuZHMgQWJzdHJh|.
    " lv_base64 = lv_base64 && |Y3RUb2tlbiB7DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvbGV4ZXJfYnVmZmVyLnRzDQp2YXIgTGV4ZXJCdWZmZXIgPSBjbGFzcyB7DQogIGNvbnN0|.
    " lv_base64 = lv_base64 && |cnVjdG9yKCkgew0KICAgIHRoaXMuYnVmID0gIiI7DQogIH0NCiAgYWRkKHMpIHsNCiAgICB0aGlzLmJ1ZiA9IHRoaXMuYnVmICsgczsNCiAgICByZXR1cm4gdGhpcy5i|.
    " lv_base64 = lv_base64 && |dWY7DQogIH0NCiAgZ2V0KCkgew0KICAgIHJldHVybiB0aGlzLmJ1ZjsNCiAgfQ0KICBjbGVhcigpIHsNCiAgICB0aGlzLmJ1ZiA9ICIiOw0KICB9DQogIGNvdW50SXNF|.
    " lv_base64 = lv_base64 && |dmVuKGNoYXIpIHsNCiAgICBsZXQgY291bnQgPSAwOw0KICAgIGZvciAobGV0IGkgPSAwOyBpIDwgdGhpcy5idWYubGVuZ3RoOyBpICs9IDEpIHsNCiAgICAgIGlmICh0|.
    " lv_base64 = lv_base64 && |aGlzLmJ1Zi5jaGFyQXQoaSkgPT09IGNoYXIpIHsNCiAgICAgICAgY291bnQgKz0gMTsNCiAgICAgIH0NCiAgICB9DQogICAgcmV0dXJuIGNvdW50ICUgMiA9PT0gMDsN|.
    " lv_base64 = lv_base64 && |CiAgfQ0KfTsNCg0KLy8gLi4vY29yZS9zcmMvYWJhcC8xX2xleGVyL2xleGVyX3N0cmVhbS50cw0KdmFyIExleGVyU3RyZWFtID0gY2xhc3Mgew0KICBjb25zdHJ1Y3Rv|.
    " lv_base64 = lv_base64 && |cihyYXcpIHsNCiAgICB0aGlzLm9mZnNldCA9IC0xOw0KICAgIHRoaXMucmF3ID0gcmF3Ow0KICAgIHRoaXMucm93ID0gMDsNCiAgICB0aGlzLmNvbCA9IDA7DQogIH0N|.
    " lv_base64 = lv_base64 && |CiAgYWR2YW5jZSgpIHsNCiAgICBpZiAodGhpcy5jdXJyZW50Q2hhcigpID09PSAiXG4iKSB7DQogICAgICB0aGlzLmNvbCA9IDE7DQogICAgICB0aGlzLnJvdyA9IHRo|.
    " lv_base64 = lv_base64 && |aXMucm93ICsgMTsNCiAgICB9DQogICAgaWYgKHRoaXMub2Zmc2V0ID09PSB0aGlzLnJhdy5sZW5ndGgpIHsNCiAgICAgIHJldHVybiBmYWxzZTsNCiAgICB9DQogICAg|.
    " lv_base64 = lv_base64 && |dGhpcy5jb2wgPSB0aGlzLmNvbCArIDE7DQogICAgdGhpcy5vZmZzZXQgPSB0aGlzLm9mZnNldCArIDE7DQogICAgcmV0dXJuIHRydWU7DQogIH0NCiAgZ2V0Q29sKCkg|.
    " lv_base64 = lv_base64 && |ew0KICAgIHJldHVybiB0aGlzLmNvbDsNCiAgfQ0KICBnZXRSb3coKSB7DQogICAgcmV0dXJuIHRoaXMucm93Ow0KICB9DQogIHByZXZDaGFyKCkgew0KICAgIGlmICh0|.
    " lv_base64 = lv_base64 && |aGlzLm9mZnNldCAtIDEgPCAwKSB7DQogICAgICByZXR1cm4gIiI7DQogICAgfQ0KICAgIHJldHVybiB0aGlzLnJhdy5zdWJzdHIodGhpcy5vZmZzZXQgLSAxLCAxKTsN|.
    " lv_base64 = lv_base64 && |CiAgfQ0KICBwcmV2UHJldkNoYXIoKSB7DQogICAgaWYgKHRoaXMub2Zmc2V0IC0gMiA8IDApIHsNCiAgICAgIHJldHVybiAiIjsNCiAgICB9DQogICAgcmV0dXJuIHRo|.
    " lv_base64 = lv_base64 && |aXMucmF3LnN1YnN0cih0aGlzLm9mZnNldCAtIDIsIDIpOw0KICB9DQogIGN1cnJlbnRDaGFyKCkgew0KICAgIGlmICh0aGlzLm9mZnNldCA8IDApIHsNCiAgICAgIHJl|.
    " lv_base64 = lv_base64 && |dHVybiAiXG4iOw0KICAgIH0gZWxzZSBpZiAodGhpcy5vZmZzZXQgPj0gdGhpcy5yYXcubGVuZ3RoKSB7DQogICAgICByZXR1cm4gIiI7DQogICAgfQ0KICAgIHJldHVy|.
    " lv_base64 = lv_base64 && |biB0aGlzLnJhdy5zdWJzdHIodGhpcy5vZmZzZXQsIDEpOw0KICB9DQogIG5leHRDaGFyKCkgew0KICAgIGlmICh0aGlzLm9mZnNldCArIDIgPiB0aGlzLnJhdy5sZW5n|.
    " lv_base64 = lv_base64 && |dGgpIHsNCiAgICAgIHJldHVybiAiIjsNCiAgICB9DQogICAgcmV0dXJuIHRoaXMucmF3LnN1YnN0cih0aGlzLm9mZnNldCArIDEsIDEpOw0KICB9DQogIG5leHROZXh0|.
    " lv_base64 = lv_base64 && |Q2hhcigpIHsNCiAgICBpZiAodGhpcy5vZmZzZXQgKyAzID4gdGhpcy5yYXcubGVuZ3RoKSB7DQogICAgICByZXR1cm4gdGhpcy5uZXh0Q2hhcigpOw0KICAgIH0NCiAg|.
    " lv_base64 = lv_base64 && |ICByZXR1cm4gdGhpcy5yYXcuc3Vic3RyKHRoaXMub2Zmc2V0ICsgMSwgMik7DQogIH0NCiAgZ2V0UmF3KCkgew0KICAgIHJldHVybiB0aGlzLnJhdzsNCiAgfQ0KICBn|.
    " lv_base64 = lv_base64 && |ZXRPZmZzZXQoKSB7DQogICAgcmV0dXJuIHRoaXMub2Zmc2V0Ow0KICB9DQp9Ow0KDQovLyAuLi9jb3JlL3NyYy9hYmFwLzFfbGV4ZXIvbGV4ZXIudHMNCnZhciBMZXhl|.
    " lv_base64 = lv_base64 && |ciA9IGNsYXNzIHsNCiAgY29uc3RydWN0b3IoKSB7DQogICAgdGhpcy5Nb2RlTm9ybWFsID0gMTsNCiAgICB0aGlzLk1vZGVQaW5nID0gMjsNCiAgICB0aGlzLk1vZGVT|.
    " lv_base64 = lv_base64 && |dHIgPSAzOw0KICAgIHRoaXMuTW9kZVRlbXBsYXRlID0gNDsNCiAgICB0aGlzLk1vZGVDb21tZW50ID0gNTsNCiAgICB0aGlzLk1vZGVQcmFnbWEgPSA2Ow0KICB9DQog|.
    " lv_base64 = lv_base64 && |IHJ1bihmaWxlLCB2aXJ0dWFsKSB7DQogICAgdGhpcy52aXJ0dWFsID0gdmlydHVhbDsNCiAgICB0aGlzLnRva2VucyA9IFtdOw0KICAgIHRoaXMubSA9IHRoaXMuTW9k|.
    " lv_base64 = lv_base64 && |ZU5vcm1hbDsNCiAgICB0aGlzLnByb2Nlc3MoZmlsZS5nZXRSYXcoKSk7DQogICAgcmV0dXJuIHsgZmlsZSwgdG9rZW5zOiB0aGlzLnRva2VucyB9Ow0KICB9DQogIGFk|.
    " lv_base64 = lv_base64 && |ZCgpIHsNCiAgICBjb25zdCBzID0gdGhpcy5idWZmZXIuZ2V0KCkudHJpbSgpOw0KICAgIGlmIChzLmxlbmd0aCA+IDApIHsNCiAgICAgIGNvbnN0IGNvbCA9IHRoaXMu|.
    " lv_base64 = lv_base64 && |c3RyZWFtLmdldENvbCgpOw0KICAgICAgY29uc3Qgcm93ID0gdGhpcy5zdHJlYW0uZ2V0Um93KCk7DQogICAgICBsZXQgd2hpdGVCZWZvcmUgPSBmYWxzZTsNCiAgICAg|.
    " lv_base64 = lv_base64 && |IGlmICh0aGlzLnN0cmVhbS5nZXRPZmZzZXQoKSAtIHMubGVuZ3RoID49IDApIHsNCiAgICAgICAgY29uc3QgcHJldiA9IHRoaXMuc3RyZWFtLmdldFJhdygpLnN1YnN0|.
    " lv_base64 = lv_base64 && |cih0aGlzLnN0cmVhbS5nZXRPZmZzZXQoKSAtIHMubGVuZ3RoLCAxKTsNCiAgICAgICAgaWYgKHByZXYgPT09ICIgIiB8fCBwcmV2ID09PSAiXG4iIHx8IHByZXYgPT09|.
    " lv_base64 = lv_base64 && |ICIJIiB8fCBwcmV2ID09PSAiOiIpIHsNCiAgICAgICAgICB3aGl0ZUJlZm9yZSA9IHRydWU7DQogICAgICAgIH0NCiAgICAgIH0NCiAgICAgIGxldCB3aGl0ZUFmdGVy|.
    " lv_base64 = lv_base64 && |ID0gZmFsc2U7DQogICAgICBjb25zdCBuZXh0ID0gdGhpcy5zdHJlYW0ubmV4dENoYXIoKTsNCiAgICAgIGlmIChuZXh0ID09PSAiICIgfHwgbmV4dCA9PT0gIlxuIiB8|.
    " lv_base64 = lv_base64 && |fCBuZXh0ID09PSAiCSIgfHwgbmV4dCA9PT0gIjoiIHx8IG5leHQgPT09ICIsIiB8fCBuZXh0ID09PSAiLiIgfHwgbmV4dCA9PT0gIiIgfHwgbmV4dCA9PT0gJyInKSB7|.
    " lv_base64 = lv_base64 && |DQogICAgICAgIHdoaXRlQWZ0ZXIgPSB0cnVlOw0KICAgICAgfQ0KICAgICAgbGV0IHBvcyA9IG5ldyBQb3NpdGlvbihyb3csIGNvbCAtIHMubGVuZ3RoKTsNCiAgICAg|.
    " lv_base64 = lv_base64 && |IGlmICh0aGlzLnZpcnR1YWwpIHsNCiAgICAgICAgcG9zID0gbmV3IFZpcnR1YWxQb3NpdGlvbih0aGlzLnZpcnR1YWwsIHBvcy5nZXRSb3coKSwgcG9zLmdldENvbCgp|.
    " lv_base64 = lv_base64 && |KTsNCiAgICAgIH0NCiAgICAgIGxldCB0b2sgPSB2b2lkIDA7DQogICAgICBpZiAodGhpcy5tID09PSB0aGlzLk1vZGVDb21tZW50KSB7DQogICAgICAgIHRvayA9IG5l|.
    " lv_base64 = lv_base64 && |dyBDb21tZW50KHBvcywgcyk7DQogICAgICB9IGVsc2UgaWYgKHRoaXMubSA9PT0gdGhpcy5Nb2RlUGluZyB8fCB0aGlzLm0gPT09IHRoaXMuTW9kZVN0cikgew0KICAg|.
    " lv_base64 = lv_base64 && |ICAgICB0b2sgPSBuZXcgU3RyaW5nVG9rZW4ocG9zLCBzKTsNCiAgICAgIH0gZWxzZSBpZiAodGhpcy5tID09PSB0aGlzLk1vZGVUZW1wbGF0ZSkgew0KICAgICAgICBj|.
    " lv_base64 = lv_base64 && |b25zdCBmaXJzdCA9IHMuY2hhckF0KDApOw0KICAgICAgICBjb25zdCBsYXN0ID0gcy5jaGFyQXQocy5sZW5ndGggLSAxKTsNCiAgICAgICAgaWYgKGZpcnN0ID09PSAi|.
    " lv_base64 = lv_base64 && |fCIgJiYgbGFzdCA9PT0gInwiKSB7DQogICAgICAgICAgdG9rID0gbmV3IFN0cmluZ1RlbXBsYXRlKHBvcywgcyk7DQogICAgICAgIH0gZWxzZSBpZiAoZmlyc3QgPT09|.
    " lv_base64 = lv_base64 && |ICJ8IiAmJiBsYXN0ID09PSAieyIgJiYgd2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgIHRvayA9IG5ldyBTdHJpbmdUZW1wbGF0ZUJlZ2luKHBvcywgcyk7|.
    " lv_base64 = lv_base64 && |DQogICAgICAgIH0gZWxzZSBpZiAoZmlyc3QgPT09ICJ9IiAmJiBsYXN0ID09PSAifCIgJiYgd2hpdGVCZWZvcmUgPT09IHRydWUpIHsNCiAgICAgICAgICB0b2sgPSBu|.
    " lv_base64 = lv_base64 && |ZXcgU3RyaW5nVGVtcGxhdGVFbmQocG9zLCBzKTsNCiAgICAgICAgfSBlbHNlIGlmIChmaXJzdCA9PT0gIn0iICYmIGxhc3QgPT09ICJ7IiAmJiB3aGl0ZUFmdGVyID09|.
    " lv_base64 = lv_base64 && |PSB0cnVlICYmIHdoaXRlQmVmb3JlID09PSB0cnVlKSB7DQogICAgICAgICAgdG9rID0gbmV3IFN0cmluZ1RlbXBsYXRlTWlkZGxlKHBvcywgcyk7DQogICAgICAgIH0g|.
    " lv_base64 = lv_base64 && |ZWxzZSB7DQogICAgICAgICAgdG9rID0gbmV3IElkZW50aWZpZXIocG9zLCBzKTsNCiAgICAgICAgfQ0KICAgICAgfSBlbHNlIGlmIChzLmxlbmd0aCA+IDIgJiYgcy5z|.
    " lv_base64 = lv_base64 && |dWJzdHIoMCwgMikgPT09ICIjIyIpIHsNCiAgICAgICAgdG9rID0gbmV3IFByYWdtYShwb3MsIHMpOw0KICAgICAgfSBlbHNlIGlmIChzLmxlbmd0aCA9PT0gMSkgew0K|.
    " lv_base64 = lv_base64 && |ICAgICAgICBpZiAocyA9PT0gIi4iIHx8IHMgPT09ICIsIikgew0KICAgICAgICAgIHRvayA9IG5ldyBQdW5jdHVhdGlvbihwb3MsIHMpOw0KICAgICAgICB9IGVsc2Ug|.
    " lv_base64 = lv_base64 && |aWYgKHMgPT09ICJbIikgew0KICAgICAgICAgIGlmICh3aGl0ZUJlZm9yZSA9PT0gdHJ1ZSAmJiB3aGl0ZUFmdGVyID09PSB0cnVlKSB7DQogICAgICAgICAgICB0b2sg|.
    " lv_base64 = lv_base64 && |PSBuZXcgV0JyYWNrZXRMZWZ0Vyhwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBpZiAod2hpdGVCZWZvcmUgPT09IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5l|.
    " lv_base64 = lv_base64 && |dyBXQnJhY2tldExlZnQocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2UgaWYgKHdoaXRlQWZ0ZXIgPT09IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBCcmFj|.
    " lv_base64 = lv_base64 && |a2V0TGVmdFcocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2Ugew0KICAgICAgICAgICAgdG9rID0gbmV3IEJyYWNrZXRMZWZ0KHBvcywgcyk7DQogICAgICAgICAgfQ0K|.
    " lv_base64 = lv_base64 && |ICAgICAgICB9IGVsc2UgaWYgKHMgPT09ICIoIikgew0KICAgICAgICAgIGlmICh3aGl0ZUJlZm9yZSA9PT0gdHJ1ZSAmJiB3aGl0ZUFmdGVyID09PSB0cnVlKSB7DQog|.
    " lv_base64 = lv_base64 && |ICAgICAgICAgICB0b2sgPSBuZXcgV1BhcmVuTGVmdFcocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2UgaWYgKHdoaXRlQmVmb3JlID09PSB0cnVlKSB7DQogICAgICAg|.
    " lv_base64 = lv_base64 && |ICAgICB0b2sgPSBuZXcgV1BhcmVuTGVmdChwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBpZiAod2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9r|.
    " lv_base64 = lv_base64 && |ID0gbmV3IFBhcmVuTGVmdFcocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2Ugew0KICAgICAgICAgICAgdG9rID0gbmV3IFBhcmVuTGVmdChwb3MsIHMpOw0KICAgICAg|.
    " lv_base64 = lv_base64 && |ICAgIH0NCiAgICAgICAgfSBlbHNlIGlmIChzID09PSAiXSIpIHsNCiAgICAgICAgICBpZiAod2hpdGVCZWZvcmUgPT09IHRydWUgJiYgd2hpdGVBZnRlciA9PT0gdHJ1|.
    " lv_base64 = lv_base64 && |ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IFdCcmFja2V0UmlnaHRXKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIGlmICh3aGl0ZUJlZm9yZSA9PT0gdHJ1ZSkg|.
    " lv_base64 = lv_base64 && |ew0KICAgICAgICAgICAgdG9rID0gbmV3IFdCcmFja2V0UmlnaHQocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2UgaWYgKHdoaXRlQWZ0ZXIgPT09IHRydWUpIHsNCiAg|.
    " lv_base64 = lv_base64 && |ICAgICAgICAgIHRvayA9IG5ldyBCcmFja2V0UmlnaHRXKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBCcmFja2V0Umln|.
    " lv_base64 = lv_base64 && |aHQocG9zLCBzKTsNCiAgICAgICAgICB9DQogICAgICAgIH0gZWxzZSBpZiAocyA9PT0gIikiKSB7DQogICAgICAgICAgaWYgKHdoaXRlQmVmb3JlID09PSB0cnVlICYm|.
    " lv_base64 = lv_base64 && |IHdoaXRlQWZ0ZXIgPT09IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBXUGFyZW5SaWdodFcocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2UgaWYgKHdoaXRl|.
    " lv_base64 = lv_base64 && |QmVmb3JlID09PSB0cnVlKSB7DQogICAgICAgICAgICB0b2sgPSBuZXcgV1BhcmVuUmlnaHQocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2UgaWYgKHdoaXRlQWZ0ZXIg|.
    " lv_base64 = lv_base64 && |PT09IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBQYXJlblJpZ2h0Vyhwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSB7DQogICAgICAgICAgICB0b2sgPSBu|.
    " lv_base64 = lv_base64 && |ZXcgUGFyZW5SaWdodChwb3MsIHMpOw0KICAgICAgICAgIH0NCiAgICAgICAgfSBlbHNlIGlmIChzID09PSAiLSIpIHsNCiAgICAgICAgICBpZiAod2hpdGVCZWZvcmUg|.
    " lv_base64 = lv_base64 && |PT09IHRydWUgJiYgd2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IFdEYXNoVyhwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBpZiAo|.
    " lv_base64 = lv_base64 && |d2hpdGVCZWZvcmUgPT09IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBXRGFzaChwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBpZiAod2hpdGVBZnRlciA9|.
    " lv_base64 = lv_base64 && |PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IERhc2hXKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBEYXNo|.
    " lv_base64 = lv_base64 && |KHBvcywgcyk7DQogICAgICAgICAgfQ0KICAgICAgICB9IGVsc2UgaWYgKHMgPT09ICIrIikgew0KICAgICAgICAgIGlmICh3aGl0ZUJlZm9yZSA9PT0gdHJ1ZSAmJiB3|.
    " lv_base64 = lv_base64 && |aGl0ZUFmdGVyID09PSB0cnVlKSB7DQogICAgICAgICAgICB0b2sgPSBuZXcgV1BsdXNXKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIGlmICh3aGl0ZUJlZm9yZSA9|.
    " lv_base64 = lv_base64 && |PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IFdQbHVzKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIGlmICh3aGl0ZUFmdGVyID09PSB0cnVlKSB7DQog|.
    " lv_base64 = lv_base64 && |ICAgICAgICAgICB0b2sgPSBuZXcgUGx1c1cocG9zLCBzKTsNCiAgICAgICAgICB9IGVsc2Ugew0KICAgICAgICAgICAgdG9rID0gbmV3IFBsdXMocG9zLCBzKTsNCiAg|.
    " lv_base64 = lv_base64 && |ICAgICAgICB9DQogICAgICAgIH0gZWxzZSBpZiAocyA9PT0gIkAiKSB7DQogICAgICAgICAgaWYgKHdoaXRlQmVmb3JlID09PSB0cnVlICYmIHdoaXRlQWZ0ZXIgPT09|.
    " lv_base64 = lv_base64 && |IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBXQXRXKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIGlmICh3aGl0ZUJlZm9yZSA9PT0gdHJ1ZSkgew0KICAg|.
    " lv_base64 = lv_base64 && |ICAgICAgICAgdG9rID0gbmV3IFdBdChwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBpZiAod2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0g|.
    " lv_base64 = lv_base64 && |bmV3IEF0Vyhwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSB7DQogICAgICAgICAgICB0b2sgPSBuZXcgQXQocG9zLCBzKTsNCiAgICAgICAgICB9DQogICAgICAgIH0N|.
    " lv_base64 = lv_base64 && |CiAgICAgIH0gZWxzZSBpZiAocy5sZW5ndGggPT09IDIpIHsNCiAgICAgICAgaWYgKHMgPT09ICItPiIpIHsNCiAgICAgICAgICBpZiAod2hpdGVCZWZvcmUgPT09IHRy|.
    " lv_base64 = lv_base64 && |dWUgJiYgd2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IFdJbnN0YW5jZUFycm93Vyhwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBp|.
    " lv_base64 = lv_base64 && |ZiAod2hpdGVCZWZvcmUgPT09IHRydWUpIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBXSW5zdGFuY2VBcnJvdyhwb3MsIHMpOw0KICAgICAgICAgIH0gZWxzZSBpZiAo|.
    " lv_base64 = lv_base64 && |d2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IEluc3RhbmNlQXJyb3dXKHBvcywgcyk7DQogICAgICAgICAgfSBlbHNlIHsNCiAgICAg|.
    " lv_base64 = lv_base64 && |ICAgICAgIHRvayA9IG5ldyBJbnN0YW5jZUFycm93KHBvcywgcyk7DQogICAgICAgICAgfQ0KICAgICAgICB9IGVsc2UgaWYgKHMgPT09ICI9PiIpIHsNCiAgICAgICAg|.
    " lv_base64 = lv_base64 && |ICBpZiAod2hpdGVCZWZvcmUgPT09IHRydWUgJiYgd2hpdGVBZnRlciA9PT0gdHJ1ZSkgew0KICAgICAgICAgICAgdG9rID0gbmV3IFdTdGF0aWNBcnJvd1cocG9zLCBz|.
    " lv_base64 = lv_base64 && |KTsNCiAgICAgICAgICB9IGVsc2UgaWYgKHdoaXRlQmVmb3JlID09PSB0cnVlKSB7DQogICAgICAgICAgICB0b2sgPSBuZXcgV1N0YXRpY0Fycm93KHBvcywgcyk7DQog|.
    " lv_base64 = lv_base64 && |ICAgICAgICAgfSBlbHNlIGlmICh3aGl0ZUFmdGVyID09PSB0cnVlKSB7DQogICAgICAgICAgICB0b2sgPSBuZXcgU3RhdGljQXJyb3dXKHBvcywgcyk7DQogICAgICAg|.
    " lv_base64 = lv_base64 && |ICAgfSBlbHNlIHsNCiAgICAgICAgICAgIHRvayA9IG5ldyBTdGF0aWNBcnJvdyhwb3MsIHMpOw0KICAgICAgICAgIH0NCiAgICAgICAgfQ0KICAgICAgfQ0KICAgICAg|.
    " lv_base64 = lv_base64 && |aWYgKHRvayA9PT0gdm9pZCAwKSB7DQogICAgICAgIHRvayA9IG5ldyBJZGVudGlmaWVyKHBvcywgcyk7DQogICAgICB9DQogICAgICB0aGlzLnRva2Vucy5wdXNoKHRv|.
    " lv_base64 = lv_base64 && |ayk7DQogICAgfQ0KICAgIHRoaXMuYnVmZmVyLmNsZWFyKCk7DQogIH0NCiAgcHJvY2VzcyhyYXcpIHsNCiAgICB0aGlzLnN0cmVhbSA9IG5ldyBMZXhlclN0cmVhbShy|.
    " lv_base64 = lv_base64 && |YXcucmVwbGFjZSgvXHIvZywgIiIpKTsNCiAgICB0aGlzLmJ1ZmZlciA9IG5ldyBMZXhlckJ1ZmZlcigpOw0KICAgIGNvbnN0IHNwbGl0cyA9IHt9Ow0KICAgIHNwbGl0|.
    " lv_base64 = lv_base64 && |c1siICJdID0gdHJ1ZTsNCiAgICBzcGxpdHNbIjoiXSA9IHRydWU7DQogICAgc3BsaXRzWyIuIl0gPSB0cnVlOw0KICAgIHNwbGl0c1siLCJdID0gdHJ1ZTsNCiAgICBz|.
    " lv_base64 = lv_base64 && |cGxpdHNbIi0iXSA9IHRydWU7DQogICAgc3BsaXRzWyIrIl0gPSB0cnVlOw0KICAgIHNwbGl0c1siKCJdID0gdHJ1ZTsNCiAgICBzcGxpdHNbIikiXSA9IHRydWU7DQog|.
    " lv_base64 = lv_base64 && |ICAgc3BsaXRzWyJbIl0gPSB0cnVlOw0KICAgIHNwbGl0c1siXSJdID0gdHJ1ZTsNCiAgICBzcGxpdHNbIgkiXSA9IHRydWU7DQogICAgc3BsaXRzWyJcbiJdID0gdHJ1|.
    " lv_base64 = lv_base64 && |ZTsNCiAgICBjb25zdCBidWZzID0ge307DQogICAgYnVmc1siLiJdID0gdHJ1ZTsNCiAgICBidWZzWyIsIl0gPSB0cnVlOw0KICAgIGJ1ZnNbIjoiXSA9IHRydWU7DQog|.
    " lv_base64 = lv_base64 && |ICAgYnVmc1siKCJdID0gdHJ1ZTsNCiAgICBidWZzWyIpIl0gPSB0cnVlOw0KICAgIGJ1ZnNbIlsiXSA9IHRydWU7DQogICAgYnVmc1siXSJdID0gdHJ1ZTsNCiAgICBi|.
    " lv_base64 = lv_base64 && |dWZzWyIrIl0gPSB0cnVlOw0KICAgIGJ1ZnNbIkAiXSA9IHRydWU7DQogICAgZm9yICg7IDsgKSB7DQogICAgICBjb25zdCBjdXJyZW50ID0gdGhpcy5zdHJlYW0uY3Vy|.
    " lv_base64 = lv_base64 && |cmVudENoYXIoKTsNCiAgICAgIGNvbnN0IGJ1ZiA9IHRoaXMuYnVmZmVyLmFkZChjdXJyZW50KTsNCiAgICAgIGNvbnN0IGFoZWFkID0gdGhpcy5zdHJlYW0ubmV4dENo|.
    " lv_base64 = lv_base64 && |YXIoKTsNCiAgICAgIGNvbnN0IGFhaGVhZCA9IHRoaXMuc3RyZWFtLm5leHROZXh0Q2hhcigpOw0KICAgICAgaWYgKHRoaXMubSA9PT0gdGhpcy5Nb2RlTm9ybWFsKSB7|.
    " lv_base64 = lv_base64 && |DQogICAgICAgIGlmIChzcGxpdHNbYWhlYWRdKSB7DQogICAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgfSBlbHNlIGlmIChhaGVhZCA9PT0gIiciKSB7DQogICAg|.
    " lv_base64 = lv_base64 && |ICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgICB0aGlzLm0gPSB0aGlzLk1vZGVTdHI7DQogICAgICAgIH0gZWxzZSBpZiAoYWhlYWQgPT09ICJ8IiB8fCBhaGVhZCA9|.
    " lv_base64 = lv_base64 && |PT0gIn0iKSB7DQogICAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgICB0aGlzLm0gPSB0aGlzLk1vZGVUZW1wbGF0ZTsNCiAgICAgICAgfSBlbHNlIGlmIChhaGVh|.
    " lv_base64 = lv_base64 && |ZCA9PT0gImAiKSB7DQogICAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgICB0aGlzLm0gPSB0aGlzLk1vZGVQaW5nOw0KICAgICAgICB9IGVsc2UgaWYgKGFhaGVh|.
    " lv_base64 = lv_base64 && |ZCA9PT0gIiMjIikgew0KICAgICAgICAgIHRoaXMuYWRkKCk7DQogICAgICAgICAgdGhpcy5tID0gdGhpcy5Nb2RlUHJhZ21hOw0KICAgICAgICB9IGVsc2UgaWYgKGFo|.
    " lv_base64 = lv_base64 && |ZWFkID09PSAnIicgfHwgYWhlYWQgPT09ICIqIiAmJiBjdXJyZW50ID09PSAiXG4iKSB7DQogICAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgICB0aGlzLm0gPSB0|.
    " lv_base64 = lv_base64 && |aGlzLk1vZGVDb21tZW50Ow0KICAgICAgICB9IGVsc2UgaWYgKGFoZWFkID09PSAiQCIgJiYgYnVmLnRyaW0oKS5sZW5ndGggPT09IDApIHsNCiAgICAgICAgICB0aGlz|.
    " lv_base64 = lv_base64 && |LmFkZCgpOw0KICAgICAgICB9IGVsc2UgaWYgKGFhaGVhZCA9PT0gIi0+IiB8fCBhYWhlYWQgPT09ICI9PiIpIHsNCiAgICAgICAgICB0aGlzLmFkZCgpOw0KICAgICAg|.
    " lv_base64 = lv_base64 && |ICB9IGVsc2UgaWYgKGN1cnJlbnQgPT09ICI+IiAmJiBhaGVhZCAhPT0gIiAiICYmICh0aGlzLnN0cmVhbS5wcmV2Q2hhcigpID09PSAiLSIgfHwgdGhpcy5zdHJlYW0u|.
    " lv_base64 = lv_base64 && |cHJldkNoYXIoKSA9PT0gIj0iKSkgew0KICAgICAgICAgIHRoaXMuYWRkKCk7DQogICAgICAgIH0gZWxzZSBpZiAoYnVmLmxlbmd0aCA9PT0gMSAmJiAoYnVmc1tidWZd|.
    " lv_base64 = lv_base64 && |IHx8IGJ1ZiA9PT0gIi0iICYmIGFoZWFkICE9PSAiPiIpKSB7DQogICAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgfQ0KICAgICAgfSBlbHNlIGlmICh0aGlzLm0g|.
    " lv_base64 = lv_base64 && |PT09IHRoaXMuTW9kZVByYWdtYSAmJiAoYWhlYWQgPT09ICIsIiB8fCBhaGVhZCA9PT0gIjoiIHx8IGFoZWFkID09PSAiLiIgfHwgYWhlYWQgPT09ICIgIiB8fCBhaGVh|.
    " lv_base64 = lv_base64 && |ZCA9PT0gIlxuIikpIHsNCiAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgdGhpcy5tID0gdGhpcy5Nb2RlTm9ybWFsOw0KICAgICAgfSBlbHNlIGlmICh0aGlzLm0g|.
    " lv_base64 = lv_base64 && |PT09IHRoaXMuTW9kZVBpbmcgJiYgYnVmLmxlbmd0aCA+IDEgJiYgY3VycmVudCA9PT0gImAiICYmIGFhaGVhZCAhPT0gImBgIiAmJiBhaGVhZCAhPT0gImAiICYmIHRo|.
    " lv_base64 = lv_base64 && |aXMuYnVmZmVyLmNvdW50SXNFdmVuKCJgIikpIHsNCiAgICAgICAgdGhpcy5hZGQoKTsNCiAgICAgICAgaWYgKGFoZWFkID09PSBgImApIHsNCiAgICAgICAgICB0aGlz|.
    " lv_base64 = lv_base64 && |Lm0gPSB0aGlzLk1vZGVDb21tZW50Ow0KICAgICAgICB9IGVsc2Ugew0KICAgICAgICAgIHRoaXMubSA9IHRoaXMuTW9kZU5vcm1hbDsNCiAgICAgICAgfQ0KICAgICAg|.
    " lv_base64 = lv_base64 && |fSBlbHNlIGlmICh0aGlzLm0gPT09IHRoaXMuTW9kZVRlbXBsYXRlICYmIGJ1Zi5sZW5ndGggPiAxICYmIChjdXJyZW50ID09PSAifCIgfHwgY3VycmVudCA9PT0gInsi|.
    " lv_base64 = lv_base64 && |KSAmJiAodGhpcy5zdHJlYW0ucHJldkNoYXIoKSAhPT0gIlxcIiB8fCB0aGlzLnN0cmVhbS5wcmV2UHJldkNoYXIoKSA9PT0gIlxcXFwiKSkgew0KICAgICAgICB0aGlz|.
    " lv_base64 = lv_base64 && |LmFkZCgpOw0KICAgICAgICB0aGlzLm0gPSB0aGlzLk1vZGVOb3JtYWw7DQogICAgICB9IGVsc2UgaWYgKHRoaXMubSA9PT0gdGhpcy5Nb2RlVGVtcGxhdGUgJiYgYWhl|.
    " lv_base64 = lv_base64 && |YWQgPT09ICJ9IiAmJiBjdXJyZW50ICE9PSAiXFwiKSB7DQogICAgICAgIHRoaXMuYWRkKCk7DQogICAgICB9IGVsc2UgaWYgKHRoaXMubSA9PT0gdGhpcy5Nb2RlU3Ry|.
    " lv_base64 = lv_base64 && |ICYmIGN1cnJlbnQgPT09ICInIiAmJiBidWYubGVuZ3RoID4gMSAmJiBhYWhlYWQgIT09ICInJyIgJiYgYWhlYWQgIT09ICInIiAmJiB0aGlzLmJ1ZmZlci5jb3VudElz|.
    " lv_base64 = lv_base64 && |RXZlbigiJyIpKSB7DQogICAgICAgIHRoaXMuYWRkKCk7DQogICAgICAgIGlmIChhaGVhZCA9PT0gJyInKSB7DQogICAgICAgICAgdGhpcy5tID0gdGhpcy5Nb2RlQ29t|.
    " lv_base64 = lv_base64 && |bWVudDsNCiAgICAgICAgfSBlbHNlIHsNCiAgICAgICAgICB0aGlzLm0gPSB0aGlzLk1vZGVOb3JtYWw7DQogICAgICAgIH0NCiAgICAgIH0gZWxzZSBpZiAoYWhlYWQg|.
    " lv_base64 = lv_base64 && |PT09ICJcbiIgJiYgdGhpcy5tICE9PSB0aGlzLk1vZGVUZW1wbGF0ZSkgew0KICAgICAgICB0aGlzLmFkZCgpOw0KICAgICAgICB0aGlzLm0gPSB0aGlzLk1vZGVOb3Jt|.
    " lv_base64 = lv_base64 && |YWw7DQogICAgICB9IGVsc2UgaWYgKHRoaXMubSA9PT0gdGhpcy5Nb2RlVGVtcGxhdGUgJiYgY3VycmVudCA9PT0gIlxuIikgew0KICAgICAgICB0aGlzLmFkZCgpOw0K|.
    " lv_base64 = lv_base64 && |ICAgICAgfQ0KICAgICAgaWYgKCF0aGlzLnN0cmVhbS5hZHZhbmNlKCkpIHsNCiAgICAgICAgYnJlYWs7DQogICAgICB9DQogICAgfQ0KICAgIHRoaXMuYWRkKCk7DQog|.
    " lv_base64 = lv_base64 && |IH0NCn07DQoNCi8vIHNyYy9pbmRleC50cw0KZnVuY3Rpb24gbWFpbihmaWxlbmFtZSwgY29kZSkgew0KICBjb25zdCBmaWxlID0gbmV3IE1lbW9yeUZpbGUoZmlsZW5h|.
    " lv_base64 = lv_base64 && |bWUsIGNvZGUpOw0KICBjb25zdCBsZXhlciA9IG5ldyBMZXhlcigpOw0KICBjb25zdCByZXN1bHQgPSBsZXhlci5ydW4oZmlsZSk7DQogIHJldHVybiBKU09OLnN0cmlu|.
    " lv_base64 = lv_base64 && |Z2lmeShyZXN1bHQpOw0KfQ0KDQptYWluKCJmb28iLCAiYmFyIik7|.
    " DATA(lv_code) = cl_http_utility=>decode_base64( lv_base64 ).
    DATA(lv_code) = |2 + 2|.

    DATA(lo_code_ptr) = string_to_vm( lv_code ).
    DATA(lo_filename_ptr) = string_to_vm( |test.js| ).
    lt_result = gi_wasm->execute_function_export(
      iv_name       = 'QTS_Eval'
      it_parameters = VALUE #(
        ( lv_context_ptr )
        ( lo_code_ptr )
        ( zcl_wasm_i32=>from_signed( strlen( lv_code ) ) )
        ( lo_filename_ptr )
        ( zcl_wasm_i32=>from_signed( 0 ) )
        ( zcl_wasm_i32=>from_signed( 0 ) ) ) ).
    DATA(lv_result_ptr) = lt_result[ 1 ].

* (param i32 i32) (result i32)
    lt_result = gi_wasm->execute_function_export(
      iv_name       = 'QTS_ResolveException'
      it_parameters = VALUE #(
        ( lv_context_ptr )
        ( lv_result_ptr ) ) ).
    DATA(lv_exception_ptr) = CAST zcl_wasm_i32( lt_result[ 1 ] ).
    IF lv_exception_ptr->mv_value > 0.
      WRITE / |Exception: { lv_exception_ptr->mv_value }|.
      lv_result_ptr = lv_exception_ptr.
    ENDIF.

* (param i32 i32) (result i32)
    lt_result = gi_wasm->execute_function_export(
        iv_name       = 'QTS_Dump'
        it_parameters = VALUE #(
          ( lv_context_ptr )
          ( lv_result_ptr ) ) ).
    DATA(lv_char_ptr) = CAST zcl_wasm_i32( lt_result[ 1 ] ).

    WRITE / |Result: { string_from_vm( lv_char_ptr ) }|.

    GET RUN TIME FIELD lv_end.
    DATA(lv_runtime) = ( lv_end - lv_start ) / 1000.
    WRITE / |{ lv_runtime }s running QuickJS|.
    rv_json = '{"parsing": "' && lv_parsing && '", "runtime": "' && lv_runtime && '"}'.

  ENDMETHOD.

  METHOD string_from_vm.

    DATA lv_offset TYPE int8.
    DATA lv_hex    TYPE x LENGTH 1.
    DATA lv_xstr   TYPE xstring.

    lv_offset = io_pointer->mv_value.

    DATA(lo_memory) = gi_wasm->get_memory( ).
    DO.
      lv_hex = lo_memory->mi_linear->get(
        iv_offset = lv_offset
        iv_length = 1 ).

      IF lv_hex = gc_null.
        EXIT.
      ENDIF.
      lv_offset = lv_offset + 1.
      CONCATENATE lv_xstr lv_hex INTO lv_xstr IN BYTE MODE.
    ENDDO.

    rv_string = cl_abap_codepage=>convert_from( lv_xstr ).

  ENDMETHOD.

  METHOD string_to_vm.

    DATA lv_xstr TYPE xstring.

    DATA(lv_length) = strlen( iv_string ) + 1.

    DATA(lt_result) = gi_wasm->execute_function_export(
      iv_name       = 'malloc'
      it_parameters = VALUE #(
        ( zcl_wasm_i32=>from_signed( lv_length ) ) ) ).

    ro_pointer = CAST zcl_wasm_i32( lt_result[ 1 ] ).

    lv_xstr = cl_abap_codepage=>convert_to( iv_string ).
    CONCATENATE lv_xstr gc_null INTO lv_xstr IN BYTE MODE.

    DATA(lo_memory) = gi_wasm->get_memory( ).
    lo_memory->mi_linear->set(
      iv_offset = CONV #( ro_pointer->mv_value )
      iv_bytes  = lv_xstr ).

  ENDMETHOD.

ENDCLASS.
