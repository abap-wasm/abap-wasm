CLASS zcl_wasm_i32_ctz DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_ctz IMPLEMENTATION.

  METHOD parse.
* todo: singletons?
    ri_instruction = NEW zcl_wasm_i32_ctz( ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-ictz-mathrm-ictz-n-i

    DATA(lv_int) = io_memory->stack_pop_i32( )->get_unsigned( ).

    DATA(lv_zeros) = 0.
    DO 32 TIMES.
      IF lv_int MOD 2 = 0.
        lv_int = lv_int / 2.
        lv_zeros = lv_zeros + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_zeros ) ).

  ENDMETHOD.

ENDCLASS.
