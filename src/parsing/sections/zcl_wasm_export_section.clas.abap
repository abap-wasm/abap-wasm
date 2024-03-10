CLASS zcl_wasm_export_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zif_wasm_module=>ty_exports
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_export_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-exportsec

    DATA ls_result TYPE zif_wasm_module=>ty_export.

    DATA(lv_count) = io_body->shift_u32( ).

    DO lv_count TIMES.
      ls_result-name = io_body->shift_utf8( ).
      ls_result-type = io_body->shift( 1 ).

      ASSERT ls_result-type = zif_wasm_types=>c_export_type-func
        OR ls_result-type = zif_wasm_types=>c_export_type-table
        OR ls_result-type = zif_wasm_types=>c_export_type-mem
        OR ls_result-type = zif_wasm_types=>c_export_type-global.

      ls_result-index = io_body->shift_u32( ).

      INSERT ls_result INTO TABLE rt_results.
* todo, whats the thing with empty/initial names?
      IF sy-subrc <> 0 AND ls_result-name IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |duplicate export name: { ls_result-name }|.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
