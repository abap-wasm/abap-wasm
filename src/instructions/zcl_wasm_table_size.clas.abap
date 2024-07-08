CLASS zcl_wasm_table_size DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_tableidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.
  PRIVATE SECTION.
    DATA mv_tableidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_table_size IMPLEMENTATION.

  METHOD constructor.
    mv_tableidx = iv_tableidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_table_size(
      iv_tableidx = io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-table-mathsf-table-size-x

    DATA(lv_size) = io_memory->table_size( CONV #( mv_tableidx ) ).

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_size ) ).
  ENDMETHOD.

ENDCLASS.
