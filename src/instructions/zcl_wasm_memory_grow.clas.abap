CLASS zcl_wasm_memory_grow DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_memory_grow IMPLEMENTATION.

  METHOD parse.
    IF io_body->shift( 1 ) <> '00'.
      RAISE EXCEPTION NEW zcx_wasm( text = |zcl_wasm_memory_grow->parse()| ).
    ENDIF.
    ri_instruction = NEW zcl_wasm_memory_grow( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_memory_grow' ).
  ENDMETHOD.

ENDCLASS.
