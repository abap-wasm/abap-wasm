CLASS cl_quickjs_env DEFINITION PUBLIC.
  PUBLIC SECTION.
* emscripten compatible shim?
    INTERFACES zif_wasm_module.
    METHODS constructor.
  PRIVATE SECTION.
    CONSTANTS gc_null TYPE x LENGTH 1 VALUE '00'.
    DATA mo_memory    TYPE REF TO zcl_wasm_memory.
    DATA mt_functions TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS read_string
      IMPORTING
        iv_pointer TYPE int8
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS cl_quickjs_env IMPLEMENTATION.

  METHOD constructor.
    INSERT '__assert_fail' INTO TABLE mt_functions.
    INSERT '__syscall_dup' INTO TABLE mt_functions.
    INSERT '__syscall_mkdirat' INTO TABLE mt_functions.
    INSERT '__syscall_openat' INTO TABLE mt_functions.
    INSERT '__syscall_stat64' INTO TABLE mt_functions.
    INSERT '_emscripten_get_now_is_monotonic' INTO TABLE mt_functions.
    INSERT '_emscripten_sanitizer_get_option' INTO TABLE mt_functions.
    INSERT '_emscripten_sanitizer_use_colors' INTO TABLE mt_functions.
    INSERT '_localtime_js' INTO TABLE mt_functions.
    INSERT '_mmap_js' INTO TABLE mt_functions.
    INSERT '_munmap_js' INTO TABLE mt_functions.
    INSERT '_tzset_js' INTO TABLE mt_functions.
    INSERT 'abort' INTO TABLE mt_functions.
    INSERT 'emscripten_date_now' INTO TABLE mt_functions.
    INSERT 'emscripten_get_heap_max' INTO TABLE mt_functions.
    INSERT 'emscripten_get_module_name' INTO TABLE mt_functions.
    INSERT 'emscripten_get_now' INTO TABLE mt_functions.
    INSERT 'emscripten_memcpy_js' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_column' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_file' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_function' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_line' INTO TABLE mt_functions.
    INSERT 'emscripten_resize_heap' INTO TABLE mt_functions.
    INSERT 'emscripten_stack_snapshot' INTO TABLE mt_functions.
    INSERT 'emscripten_stack_unwind_buffer' INTO TABLE mt_functions.
    INSERT 'qts_host_call_function' INTO TABLE mt_functions.
    INSERT 'qts_host_interrupt_handler' INTO TABLE mt_functions.
    INSERT 'qts_host_load_module_source' INTO TABLE mt_functions.
    INSERT 'qts_host_normalize_module' INTO TABLE mt_functions.
  ENDMETHOD.

  METHOD read_string.
    DATA lv_xstr TYPE xstring.
    DATA lv_hex  TYPE x LENGTH 1 VALUE '01'.

    DATA(li_linear) = mo_memory->get_linear( ).
    DATA(lv_pointer) = iv_pointer.

    WHILE lv_hex <> gc_null.
      lv_hex = li_linear->get(
        iv_length = 1
        iv_offset = lv_pointer ).
      lv_xstr = lv_xstr && lv_hex.
      lv_pointer = lv_pointer + 1.
      ASSERT xstrlen( lv_xstr ) < 1000. " avoid infinite loop
    ENDWHILE.
    rv_string = cl_abap_codepage=>convert_from( lv_xstr ).
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.
    DATA lv_xstr TYPE xstring.
    DATA(li_linear) = mo_memory->get_linear( ).

    WRITE / iv_name.

    CASE iv_name.
      WHEN 'emscripten_get_module_name'.
* (param i32 i32) (result i32)
* input: pointer + max length
* return: bytes written to pointer?
        DATA(lv_max) = CAST zcl_wasm_i32( it_parameters[ 1 ] )->get_signed( ).
        DATA(lv_pointer) = CAST zcl_wasm_i32( it_parameters[ 2 ] )->get_signed( ).
        WRITE / lv_pointer.

        lv_xstr = cl_abap_codepage=>convert_to( 'hello.wasm' ).
        CONCATENATE lv_xstr gc_null INTO lv_xstr IN BYTE MODE.
        li_linear->set(
          iv_bytes  = lv_xstr
          iv_offset = CONV #( lv_pointer ) ).
        INSERT zcl_wasm_i32=>from_signed( xstrlen( lv_xstr ) ) INTO rt_results.
      WHEN '_emscripten_sanitizer_get_option'.
* (param i32) (result i32)
* input: pointer to string?
* output: pointer to string?
        lv_pointer = CAST zcl_wasm_i32( it_parameters[ 1 ] )->get_signed( ).
        DATA(lv_str) = read_string( CONV #( lv_pointer ) ).
        WRITE / lv_str.
* todo, return malloc'ed string pointer? by calling the malloc inside the wasm?
* ya, https://github.com/emscripten-core/emscripten/blob/d0c4878b899c6b597c2291ce6ff2734bb9136a8d/src/library_strings.js#L479
        INSERT zcl_wasm_i32=>from_signed( 0 ) INTO rt_results.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wasm
          EXPORTING
            text = |cl_quickjs_env: execute_function_export "{ iv_name }"|.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    READ TABLE mt_functions WITH KEY table_line = iv_name INTO DATA(lv_name).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'cl_quickjs_env: get_export_by_name'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    mo_memory = NEW zcl_wasm_memory( ).
* initial memory in JS is 16mb = 256 pages
    mo_memory->set_linear( NEW zcl_wasm_memory_linear(
      iv_min = 256 " todo, can this be reduced?
      iv_max = 1000 ) ).
    ri_module ?= me.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.

ENDCLASS.
