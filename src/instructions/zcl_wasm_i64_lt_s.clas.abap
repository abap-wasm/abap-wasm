CLASS zcl_wasm_i64_lt_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_lt_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_lt_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#t-mathsf-xref-syntax-instructions-syntax-relop-mathit-relop

    "##feature-start=debug
    IF io_memory->mi_stack->get_length( ) < 2.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt_s, expected two variables on stack'.
    ENDIF.
    "##feature-end=debug

    TRY.
        DATA(lo_val1) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) ).
        DATA(lo_val2) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'lt_s, wrong types on stack'.
    ENDTRY.

    DATA(lv_result) = 0.
    IF lo_val1->get_signed( ) > lo_val2->get_signed( ).
      lv_result = 1.
    ENDIF.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_result ) ).

  ENDMETHOD.

ENDCLASS.
