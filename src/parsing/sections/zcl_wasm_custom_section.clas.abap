CLASS zcl_wasm_custom_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zcl_wasm_module=>ty_types
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_custom_section IMPLEMENTATION.

  METHOD parse.
* https://webassembly.github.io/spec/core/binary/modules.html#binary-customsec
* https://webassembly.github.io/spec/core/appendix/custom.html

* "ignored by the WebAssembly semantics", but must validate utf8, see utf8-custom-section-id.wast

    DATA(lv_str) = io_body->shift_utf8( ).
    IF lv_str <> 'name'.
      RETURN.
    ENDIF.

    WHILE io_body->get_length( ) > 0.
      DATA(lv_byte) = io_body->shift( 1 ).
      CASE lv_byte.
        WHEN '00'.
* module name
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |custom section: todo, module names|.
        WHEN '01'.
* function names
          DATA(lv_size) = io_body->shift_u32( ).

          DATA(lv_len) = io_body->shift_u32( ).
          DO lv_len TIMES.
            io_body->shift_u32( ).
            io_body->shift_utf8( ).
          ENDDO.
        WHEN '02'.
* local names
          lv_size = io_body->shift_u32( ).
* for now, just skip it,
          io_body->shift( lv_size ).
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |unexpected custom subsection id|.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
