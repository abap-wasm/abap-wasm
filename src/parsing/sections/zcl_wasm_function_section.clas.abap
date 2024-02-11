CLASS zcl_wasm_function_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zcl_wasm_module=>ty_functions
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_function_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec

    DO io_body->shift_u32( ) TIMES.
      APPEND io_body->shift_u32( ) TO rt_results.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
