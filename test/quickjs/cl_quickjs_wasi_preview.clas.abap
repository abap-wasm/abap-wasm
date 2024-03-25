CLASS cl_quickjs_wasi_preview DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_module.
    METHODS constructor IMPORTING io_memory TYPE REF TO zcl_wasm_memory.
  PRIVATE SECTION.
    DATA mo_memory    TYPE REF TO zcl_wasm_memory.
    DATA mt_functions TYPE STANDARD TABLE OF string WITH EMPTY KEY.
ENDCLASS.

CLASS cl_quickjs_wasi_preview IMPLEMENTATION.

  METHOD constructor.
    INSERT 'fd_close' INTO TABLE mt_functions.
    INSERT 'fd_read' INTO TABLE mt_functions.
    INSERT 'fd_write' INTO TABLE mt_functions.
    INSERT 'proc_exit' INTO TABLE mt_functions.
    INSERT 'fd_seek' INTO TABLE mt_functions.

    mo_memory = io_memory.
  ENDMETHOD.

  METHOD zif_wasm_module~execute_function_export.
    DATA lv_xstr TYPE xstring.
    DATA(li_linear) = mo_memory->get_linear( ).

    CASE iv_name.
      WHEN 'fd_write'.
* https://github.com/tinygo-org/tinygo/blob/6384ecace093df2d0b93915886954abfc4ecfe01/targets/wasm_exec.js#L242
* (param i32 i32 i32 i32) (result i32)
        DATA(lv_nwritten) = CAST zcl_wasm_i32( it_parameters[ 1 ] ).
        DATA(lv_size) = CAST zcl_wasm_i32( it_parameters[ 2 ] ).
        DATA(lv_iovs) = CAST zcl_wasm_i32( it_parameters[ 3 ] ).
        DATA(lv_fd) = CAST zcl_wasm_i32( it_parameters[ 4 ] ).
        WRITE / |fd, { lv_fd->get_signed( ) }|.
        WRITE / |iovs, { lv_iovs->get_signed( ) }|.
        WRITE / |size, { lv_size->get_signed( ) }|.
        WRITE / |nwritten, { lv_nwritten->get_signed( ) }|.

        DO lv_size TIMES.
          DATA(lv_ptr) = lv_iovs + 8 * ( sy-index - 1 ).
          DATA(lv_len) = lv_iovs + 4 + 8 * ( sy-index - 1 ).

          lv_xstr = li_linear->get(
            iv_length = lv_len
            iv_offset = lv_ptr ).
          WRITE / cl_abap_codepage=>convert_from( lv_xstr ).
        ENDDO.

        ASSERT 1 = 2.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wasm
          EXPORTING
            text = |cl_quickjs_wasi_preview: execute_function_export "{ iv_name }"|.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_wasm_module~get_export_by_name.
    READ TABLE mt_functions WITH KEY table_line = iv_name INTO DATA(lv_name).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |cl_quickjs_wasi_preview: get_export_by_name "{ iv_name }"|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_module~instantiate.
    ri_module ?= me.
  ENDMETHOD.

  METHOD zif_wasm_module~get_memory.
    RAISE EXCEPTION TYPE zcx_wasm
      EXPORTING
        text = 'cl_quickjs_wasi_preview: get_memory'.
  ENDMETHOD.

ENDCLASS.
