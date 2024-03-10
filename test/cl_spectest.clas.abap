CLASS cl_spectest DEFINITION PUBLIC.
  PUBLIC SECTION.
* https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#spectest-host-module
    INTERFACES zif_wasm_module.
    METHODS constructor.
  PRIVATE SECTION.
    DATA mt_functions TYPE STANDARD TABLE OF string WITH EMPTY KEY.
ENDCLASS.

CLASS cl_spectest IMPLEMENTATION.

  METHOD constructor.
    INSERT 'print' INTO TABLE mt_functions.
    INSERT 'print_i32' INTO TABLE mt_functions.
    INSERT 'print_i64' INTO TABLE mt_functions.
    INSERT 'print_f32' INTO TABLE mt_functions.
    INSERT 'print_f64' INTO TABLE mt_functions.
    INSERT 'print_i32_f32' INTO TABLE mt_functions.
    INSERT 'print_f64_f64' INTO TABLE mt_functions.
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.
* for now, do nothing,
    RETURN.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    READ TABLE mt_functions WITH KEY table_line = iv_name INTO DATA(lv_name).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'cl_spectest: get_export_by_name'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_spectest: instantiate'.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_spectest: get_memory'.
  ENDMETHOD.

ENDCLASS.
