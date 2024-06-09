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
        iv_pointer       TYPE int8
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS cl_quickjs_env IMPLEMENTATION.

  METHOD constructor.
* https://github.com/emscripten-core/emscripten/blob/main/src/library.js

    INSERT |__assert_fail| INTO TABLE mt_functions.
    INSERT |__syscall_dup| INTO TABLE mt_functions.
    INSERT |__syscall_mkdirat| INTO TABLE mt_functions.
    INSERT |__syscall_openat| INTO TABLE mt_functions.
    INSERT |__syscall_stat64| INTO TABLE mt_functions.
    INSERT |_emscripten_get_now_is_monotonic| INTO TABLE mt_functions.
    INSERT |_emscripten_sanitizer_get_option| INTO TABLE mt_functions.
    INSERT |_emscripten_sanitizer_use_colors| INTO TABLE mt_functions.
    INSERT |_localtime_js| INTO TABLE mt_functions.
    INSERT |_mmap_js| INTO TABLE mt_functions.
    INSERT |_munmap_js| INTO TABLE mt_functions.
    INSERT |_tzset_js| INTO TABLE mt_functions.
    INSERT |abort| INTO TABLE mt_functions.
    INSERT |emscripten_date_now| INTO TABLE mt_functions.
    INSERT |emscripten_get_heap_max| INTO TABLE mt_functions.
    INSERT |emscripten_get_module_name| INTO TABLE mt_functions.
    INSERT |emscripten_get_now| INTO TABLE mt_functions.
    INSERT |emscripten_memcpy_js| INTO TABLE mt_functions.
    INSERT |emscripten_pc_get_column| INTO TABLE mt_functions.
    INSERT |emscripten_pc_get_file| INTO TABLE mt_functions.
    INSERT |emscripten_pc_get_function| INTO TABLE mt_functions.
    INSERT |emscripten_pc_get_line| INTO TABLE mt_functions.
    INSERT |emscripten_resize_heap| INTO TABLE mt_functions.
    INSERT |emscripten_stack_snapshot| INTO TABLE mt_functions.
    INSERT |emscripten_stack_unwind_buffer| INTO TABLE mt_functions.
    INSERT |qts_host_call_function| INTO TABLE mt_functions.
    INSERT |qts_host_interrupt_handler| INTO TABLE mt_functions.
    INSERT |qts_host_load_module_source| INTO TABLE mt_functions.
    INSERT |qts_host_normalize_module| INTO TABLE mt_functions.
  ENDMETHOD.

  METHOD read_string.
    DATA lv_xstr TYPE xstring.
    DATA lv_hex  TYPE x LENGTH 1 VALUE '01'.

    DATA(li_linear) = mo_memory->mi_linear.
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
    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.
    DATA lv_xstr       TYPE xstring.
    DATA lv_seconds    TYPE f.
    DATA lv_time       TYPE timestamp.

    DATA(li_linear) = mo_memory->mi_linear.

    CASE iv_name.
      WHEN 'emscripten_memcpy_js'.
* (param i32 i32 i32)
        DATA(lv_n) = CAST zcl_wasm_i32( it_parameters[ 1 ] )->mv_value.
        DATA(lv_src) = CAST zcl_wasm_i32( it_parameters[ 2 ] )->mv_value.
        DATA(lv_dest) = CAST zcl_wasm_i32( it_parameters[ 3 ] )->mv_value.
        WRITE / |emscripten_memcpy_js: { lv_n }, { lv_src }, { lv_dest }|.

        lv_xstr = li_linear->get(
          iv_length = CONV #( lv_n )
          iv_offset = CONV #( lv_src ) ).
        lv_xstr = zcl_wasm_binary_stream=>reverse_hex( lv_xstr ).
        li_linear->set(
          iv_bytes  = lv_xstr
          iv_offset = CONV #( lv_dest ) ).
      WHEN 'emscripten_date_now'.
* (result f64)
* double, clock now in milliseconds
        GET TIME STAMP FIELD lv_time.
        lv_seconds = cl_abap_tstmp=>subtract(
          tstmp1 = lv_time
          tstmp2 = lc_epoch ).
        lv_seconds = lv_seconds * 1000.
        INSERT zcl_wasm_f64=>from_float( lv_seconds ) INTO rt_results.
      WHEN 'emscripten_get_module_name'.
* (param i32 i32) (result i32)
* input: pointer + max length
* return: bytes written to pointer?
        DATA(lv_max) = CAST zcl_wasm_i32( it_parameters[ 1 ] )->mv_value.
        DATA(lv_pointer) = CAST zcl_wasm_i32( it_parameters[ 2 ] )->mv_value.

        lv_xstr = cl_abap_codepage=>convert_to( 'hello.wasm' ).
        CONCATENATE lv_xstr gc_null INTO lv_xstr IN BYTE MODE.
        li_linear->set(
          iv_bytes  = lv_xstr
          iv_offset = CONV #( lv_pointer ) ).
        INSERT zcl_wasm_i32=>from_signed( xstrlen( lv_xstr ) ) INTO rt_results.
      WHEN 'emscripten_resize_heap'.
* https://github.com/emscripten-core/emscripten/blob/918e131fae0b5c7b1d05a5c75d7e8e676c377713/system/include/emscripten/heap.h#L27-L32
* https://github.com/emscripten-core/emscripten/blob/918e131fae0b5c7b1d05a5c75d7e8e676c377713/system/lib/standalone/standalone.c#L152
* (param i32) (result i32)
* return: 1 = success, 0 = failure?
        DATA(lv_input) = CAST zcl_wasm_i32( it_parameters[ 1 ] )->mv_value.
        DATA(lv_diff) = lv_input - mo_memory->mi_linear->size_in_bytes( ).
        DATA(lv_pages) = ceil( lv_diff / 65536 ) + 1.
        WRITE / |emscripten_resize_heap: grow { lv_pages } pages, requested diff { lv_diff }, requested size { lv_input }|.
        mo_memory->mi_linear->grow( CONV #( lv_pages ) ).
        INSERT zcl_wasm_i32=>from_signed( 1 ) INTO rt_results.
      WHEN 'emscripten_stack_unwind_buffer'.
* https://github.com/emscripten-core/emscripten/blob/faee8d3e40ec8b0905ed26d31b1d5e332509519c/src/library_stack_trace.js#L238
* (param i32 i32 i32) (result i32)
* hmm
        INSERT zcl_wasm_i32=>from_signed( 0 ) INTO rt_results.
      WHEN 'emscripten_stack_snapshot'.
* (result i32)
* https://github.com/emscripten-core/emscripten/blob/918e131fae0b5c7b1d05a5c75d7e8e676c377713/src/library.js#L2599
* hmm
        INSERT zcl_wasm_i32=>from_signed( 0 ) INTO rt_results.
      WHEN '_emscripten_sanitizer_get_option'.
* (param i32) (result i32)
* input: pointer to string?
* output: pointer to string?
        lv_pointer = CAST zcl_wasm_i32( it_parameters[ 1 ] )->mv_value.
        DATA(lv_str) = read_string( CONV #( lv_pointer ) ).
        WRITE / |emscripten_sanitizer_get_option: { lv_str }|.
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
    mo_memory->mi_linear = NEW zcl_wasm_memory_linear(
      iv_min = 256 " todo, can this be reduced?
      iv_max = 1000 ).
    ri_module ?= me.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.

ENDCLASS.
