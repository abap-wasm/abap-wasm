CLASS zcl_wasm_memory_size DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING
        !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_memory_size IMPLEMENTATION.

  METHOD parse.
    IF io_body->shift( 1 ) <> '00'.
      RAISE EXCEPTION NEW zcx_wasm( text = |zero byte expected| ).
    ENDIF.

* todo, singleton?
    ri_instruction = NEW zcl_wasm_memory_size( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-size

    DATA(lv_length) = xstrlen( io_memory->linear_get( ) ) / zcl_wasm_memory=>c_page_size.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_length ) ).

  ENDMETHOD.

ENDCLASS.
