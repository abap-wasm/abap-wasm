CLASS zcl_wasm_function_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body     TYPE REF TO zcl_wasm_binary_stream
      CHANGING
        ct_functions TYPE zcl_wasm_module=>ty_functions
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_function_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec

    DATA(lv_codeidx) = 0.
    DATA(lv_times) = io_body->shift_u32( ).
    DO lv_times TIMES.
      APPEND VALUE #(
        typeidx = io_body->shift_u32( )
        codeidx = lv_codeidx ) TO ct_functions.
      lv_codeidx = lv_codeidx + 1.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
