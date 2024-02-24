CLASS zcl_wasm_f32_min DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_f32_min IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_f32_min( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-fmin-mathrm-fmin-n-z-1-z-2

    ASSERT io_memory->stack_length( ) >= 2.

    DATA(lo_val1) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f32( io_memory->stack_pop( ) ).

    IF lo_val1->get_value( ) < lo_val2->get_value( ).
      io_memory->stack_push( lo_val1 ).
    ELSE.
      io_memory->stack_push( lo_val2 ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
