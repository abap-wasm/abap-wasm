CLASS zcl_wasm_i64_shr_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_shr_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_shr_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    DATA(lv_count) = io_memory->mi_stack->pop_i64( )->get_signed( ) MOD 64.

    DATA(li_val) = io_memory->mi_stack->pop_i64( ).
    DATA(lv_int) = li_val->get_signed( ).

    IF lv_count = 0.
      io_memory->mi_stack->push( li_val ).
    ELSE.
      DO lv_count TIMES.
        lv_int = lv_int DIV 2.
      ENDDO.
      io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_int ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
