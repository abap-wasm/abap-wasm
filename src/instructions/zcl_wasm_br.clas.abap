CLASS zcl_wasm_br DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_labelidx TYPE int8.

    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_labelidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_br IMPLEMENTATION.

  METHOD constructor.
    mv_labelidx = iv_labelidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_br( io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-l

    " DATA(lv_len) = io_memory->mi_stack->get_length( ).
    " WRITE / '@KERNEL console.dir(lv_len);'.
    " WRITE / '@KERNEL console.dir(this.mv_labelidx);'.

    cs_control-control = zif_wasm_instruction=>c_control-branch.
    cs_control-depth = mv_labelidx.
  ENDMETHOD.

ENDCLASS.
