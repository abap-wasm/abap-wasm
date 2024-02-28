CLASS zcl_wasm_memory_copy DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_memory_copy IMPLEMENTATION.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_memory_copy( ).
    ASSERT io_body->shift( 1 ) = '00'.
    ASSERT io_body->shift( 1 ) = '00'.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-copy

    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_memory_copy' ).

  ENDMETHOD.

ENDCLASS.
