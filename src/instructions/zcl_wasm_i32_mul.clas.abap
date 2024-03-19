CLASS zcl_wasm_i32_mul DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_mul IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_mul( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    DATA lv_long1 TYPE int8.
    DATA lv_long2 TYPE int8.

    DATA(lo_val1) = io_memory->mi_stack->pop_i32( ).
    DATA(lo_val2) = io_memory->mi_stack->pop_i32( ).

* dont overflow, so convert to int8s
    lv_long1 = lo_val1->get_signed( ).
    lv_long2 = lo_val2->get_signed( ).
    lv_long1 = lv_long1 * lv_long2.

    io_memory->mi_stack->push( zcl_wasm_i32=>from_int8( lv_long1 ) ).

  ENDMETHOD.

ENDCLASS.
