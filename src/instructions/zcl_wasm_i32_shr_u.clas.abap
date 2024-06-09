CLASS zcl_wasm_i32_shr_u DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_shr_u IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_shr_u( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-ishr-u-mathrm-ishr-u-n-i-1-i-2
* shift right unsigned

    DATA(lv_count) = io_memory->mi_stack->pop_i32( )->mv_value MOD 32.

    DATA(li_val) = io_memory->mi_stack->pop_i32( ).
    DATA(lv_int) = li_val->get_unsigned( ).

    IF lv_count = 0.
      io_memory->mi_stack->push( li_val ).
    ELSE.
      DO lv_count TIMES.
        lv_int = lv_int DIV 2.
      ENDDO.
      io_memory->mi_stack->push( zcl_wasm_i32=>from_unsigned( lv_int ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
