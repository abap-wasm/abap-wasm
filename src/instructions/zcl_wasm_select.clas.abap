CLASS zcl_wasm_select DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_select IMPLEMENTATION.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_select( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-parametric-mathsf-select-t-ast

    DATA lo_c TYPE REF TO zcl_wasm_i32.

    TRY.
        lo_c ?= io_memory->mi_stack->pop( ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'select: expected i32'.
    ENDTRY.

    DATA(lo_val1) = io_memory->mi_stack->pop( ).
    DATA(lo_val2) = io_memory->mi_stack->pop( ).
    IF lo_val1->get_type( ) <> lo_val2->get_type( ).
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'select: expected same type'.
    ENDIF.

    IF lo_c->mv_value = 0.
      io_memory->mi_stack->push( lo_val1 ).
    ELSE.
      io_memory->mi_stack->push( lo_val2 ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
