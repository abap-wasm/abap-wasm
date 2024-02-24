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

* todo

  ENDMETHOD.

ENDCLASS.
