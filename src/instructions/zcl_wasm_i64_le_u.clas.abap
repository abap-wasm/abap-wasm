CLASS zcl_wasm_i64_le_u DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_le_u IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_le_u( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* unsigned less than or equals, using signed comparison

    DATA lv_result TYPE abap_bool.

    DATA(lv_val1) = io_memory->mi_stack->pop_i64( )->get_signed( ).
    DATA(lv_val2) = io_memory->mi_stack->pop_i64( )->get_signed( ).

* this can probably be done with fewer comparisons
    IF lv_val1 >= 0 AND lv_val2 >= 0.
      lv_result = xsdbool( lv_val1 >= lv_val2 ).
    ELSEIF lv_val1 < 0 AND lv_val2 < 0.
      lv_result = xsdbool( lv_val1 >= lv_val2 ).
    ELSEIF lv_val1 < 0 AND lv_val2 >= 0.
      lv_result = abap_true.
    ELSE.
      lv_result = abap_false.
    ENDIF.

    IF lv_result = abap_true.
      io_memory->mi_stack->push( zcl_wasm_i32=>gc_one ).
    ELSE.
      io_memory->mi_stack->push( zcl_wasm_i32=>gc_zero ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
