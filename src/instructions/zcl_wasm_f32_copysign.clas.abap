CLASS zcl_wasm_f32_copysign DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_f32_copysign IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_f32_copysign( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-fcopysign-mathrm-fcopysign-n-z-1-z-2

    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_f32_copysign' ).
  ENDMETHOD.

ENDCLASS.
