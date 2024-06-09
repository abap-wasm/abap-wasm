CLASS zcl_wasm_f64_sub DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_f64_sub IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_f64_sub( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    "##feature-start=debug
    ASSERT io_memory->mi_stack->get_length( ) >= 2.
    "##feature-end=debug

    DATA(lo_val1) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    DATA(lo_val2) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).

    IF lo_val1->get_special( ) IS INITIAL AND lo_val2->get_special( ) IS INITIAL.
      io_memory->mi_stack->push( zcl_wasm_f64=>from_float( lo_val2->get_value( ) - lo_val1->get_value( ) ) ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'f64.sub: todo one of the values is a special value'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
