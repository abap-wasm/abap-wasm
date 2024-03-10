CLASS cl_quickjs_env DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.
ENDCLASS.

CLASS cl_quickjs_env IMPLEMENTATION.

  METHOD zif_wasm_module~execute_function_export.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_quickjs_env: execute_function_export'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.

" qts_host_call_function
" qts_host_interrupt_handler
" qts_host_load_module_source
" qts_host_normalize_module
" abort
" __assert_fail
" __syscall_dup
" emscripten_memcpy_js
" emscripten_date_now
" _emscripten_get_now_is_monotonic
" emscripten_get_now
" __syscall_openat
" __syscall_stat64
" __syscall_mkdirat
" emscripten_get_heap_max
" _tzset_js
" emscripten_resize_heap
" _emscripten_sanitizer_get_option
" emscripten_get_module_name
" emscripten_stack_snapshot
" emscripten_stack_unwind_buffer
" emscripten_pc_get_function
" emscripten_pc_get_file
" emscripten_pc_get_line
" emscripten_pc_get_column
" _emscripten_sanitizer_use_colors
" _localtime_js
" _munmap_js
" _mmap_js

    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_quickjs_env: get_export_by_name'.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    mo_memory = NEW zcl_wasm_memory( ).
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    ro_memory = mo_memory.
  ENDMETHOD.

ENDCLASS.
