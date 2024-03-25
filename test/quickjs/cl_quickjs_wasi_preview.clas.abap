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
    DATA lv_xstr    TYPE xstring.
    DATA lv_pointer TYPE i.
    DATA lv_length  TYPE i.
    DATA lv_written TYPE i.
    DATA lv_hex4    TYPE x LENGTH 4.
    DATA(li_linear) = mo_memory->get_linear( ).

    CASE iv_name.
      WHEN 'fd_write'.
* https://github.com/tinygo-org/tinygo/blob/6384ecace093df2d0b93915886954abfc4ecfe01/targets/wasm_exec.js#L242
* (param i32 i32 i32 i32) (result i32)
        DATA(lv_nwritten) = CAST zcl_wasm_i32( it_parameters[ 1 ] ).
        DATA(lv_size) = CAST zcl_wasm_i32( it_parameters[ 2 ] ).
        DATA(lv_iovs) = CAST zcl_wasm_i32( it_parameters[ 3 ] ).
        DATA(lv_fd) = CAST zcl_wasm_i32( it_parameters[ 4 ] ).
        " WRITE / |fd, { lv_fd->get_signed( ) }|.
        " WRITE / |iovs, { lv_iovs->get_signed( ) }|.
        " WRITE / |size, { lv_size->get_signed( ) }|.
        " WRITE / |nwritten, { lv_nwritten->get_signed( ) }|.

        DO lv_size->get_signed( ) TIMES.
          DATA(lv_index) = sy-index - 1.

          lv_pointer = li_linear->get(
            iv_length = 4
            iv_offset = lv_iovs->get_signed( ) + ( 8 * lv_index ) ).

          lv_length = li_linear->get(
            iv_length = 4
            iv_offset = lv_iovs->get_signed( ) + 4 + ( 8 * lv_index ) ).
          " WRITE / lv_pointer.
          " WRITE / lv_length.
          IF lv_length = 0.
            CONTINUE.
          ENDIF.

          lv_xstr = li_linear->get(
            iv_length = CONV #( lv_length )
            iv_offset = CONV #( lv_pointer ) ).
          DATA(lv_text) = cl_abap_codepage=>convert_from( zcl_wasm_binary_stream=>reverse_hex( lv_xstr ) ).
          WRITE / lv_text.
          lv_written = lv_written + strlen( lv_text ).
        ENDDO.

        " WRITE / |real written, { lv_written }|.
        lv_hex4 = lv_written.
        li_linear->set(
          iv_bytes  = zcl_wasm_binary_stream=>reverse_hex( lv_hex4 )
          iv_offset = lv_nwritten->get_signed( ) ).

        INSERT zcl_wasm_i32=>from_signed( 0 ) INTO rt_results.
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
