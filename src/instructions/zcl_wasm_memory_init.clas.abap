CLASS zcl_wasm_memory_init DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_dataidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    DATA mv_dataidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_memory_init IMPLEMENTATION.

  METHOD constructor.
    mv_dataidx = iv_dataidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_memory_init( io_body->shift_u32( ) ).
    ASSERT io_body->shift( 1 ) = '00'.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* copy passive data to memory

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-init-x

*    io_module->get_data_section( )->

    RAISE EXCEPTION NEW zcx_wasm( text = 'todo, execute instruction zcl_wasm_memory_init' ).
  ENDMETHOD.

ENDCLASS.
