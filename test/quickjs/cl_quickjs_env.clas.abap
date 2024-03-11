CLASS cl_quickjs_env DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.
    DATA mt_functions TYPE STANDARD TABLE OF string WITH EMPTY KEY.
ENDCLASS.

CLASS cl_quickjs_env IMPLEMENTATION.

  METHOD constructor.
    INSERT 'qts_host_call_function' INTO TABLE mt_functions.
    INSERT 'qts_host_interrupt_handler' INTO TABLE mt_functions.
    INSERT 'qts_host_load_module_source' INTO TABLE mt_functions.
    INSERT 'qts_host_normalize_module' INTO TABLE mt_functions.
    INSERT 'abort' INTO TABLE mt_functions.
    INSERT '__assert_fail' INTO TABLE mt_functions.
    INSERT '__syscall_dup' INTO TABLE mt_functions.
    INSERT 'emscripten_memcpy_js' INTO TABLE mt_functions.
    INSERT 'emscripten_date_now' INTO TABLE mt_functions.
    INSERT '_emscripten_get_now_is_monotonic' INTO TABLE mt_functions.
    INSERT 'emscripten_get_now' INTO TABLE mt_functions.
    INSERT '__syscall_openat' INTO TABLE mt_functions.
    INSERT '__syscall_stat64' INTO TABLE mt_functions.
    INSERT '__syscall_mkdirat' INTO TABLE mt_functions.
    INSERT 'emscripten_get_heap_max' INTO TABLE mt_functions.
    INSERT '_tzset_js' INTO TABLE mt_functions.
    INSERT 'emscripten_resize_heap' INTO TABLE mt_functions.
    INSERT '_emscripten_sanitizer_get_option' INTO TABLE mt_functions.
    INSERT 'emscripten_get_module_name' INTO TABLE mt_functions.
    INSERT 'emscripten_stack_snapshot' INTO TABLE mt_functions.
    INSERT 'emscripten_stack_unwind_buffer' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_function' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_file' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_line' INTO TABLE mt_functions.
    INSERT 'emscripten_pc_get_column' INTO TABLE mt_functions.
    INSERT '_emscripten_sanitizer_use_colors' INTO TABLE mt_functions.
    INSERT '_localtime_js' INTO TABLE mt_functions.
    INSERT '_munmap_js' INTO TABLE mt_functions.
    INSERT '_mmap_js' INTO TABLE mt_functions.
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_quickjs_env: execute_function_export'.
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
    mo_memory->set_linear( NEW zcl_wasm_memory_linear(
      iv_min = 256 " todo, can this be reduced?
      iv_max = 1000 ) ).
    ri_module ?= me.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.

ENDCLASS.
