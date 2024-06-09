CLASS zcl_wasm_table_copy DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_x TYPE int8
        iv_y TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
  PRIVATE SECTION.
    DATA mv_x TYPE int8.
    DATA mv_y TYPE int8.
ENDCLASS.

CLASS zcl_wasm_table_copy IMPLEMENTATION.

  METHOD constructor.
    mv_x = iv_x.
    mv_y = iv_y.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_table_copy(
      iv_x = io_body->shift_u32( )
      iv_y = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-table-mathsf-table-copy-x-y
    RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'todo, execute instruction zcl_wasm_table_copy'.
  ENDMETHOD.

ENDCLASS.
