CLASS zcl_wasm_memory_size DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING
        !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_memory_size IMPLEMENTATION.

  METHOD parse.
    IF io_body->shift( 1 ) <> '00'.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zero byte expected|.
    ENDIF.

    ri_instruction = NEW zcl_wasm_memory_size( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-size

    DATA(lv_size) = io_memory->mi_linear->size_in_pages( ).
    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_size ) ).

  ENDMETHOD.

ENDCLASS.
