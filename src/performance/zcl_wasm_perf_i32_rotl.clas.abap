CLASS zcl_wasm_perf_i32_rotl DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run RAISING zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_perf_i32_rotl IMPLEMENTATION.

  METHOD run.

    CONSTANTS lc_iterations TYPE i VALUE 80000.

    DATA lo_module TYPE REF TO zcl_wasm_module.
    DATA ls_control TYPE zif_wasm_instruction=>ty_control.

    DATA(li_instruction) = CAST zif_wasm_instruction( NEW zcl_wasm_i32_rotl( ) ).

    DATA(lo_memory) = NEW zcl_wasm_memory( ).

    DO lc_iterations TIMES.
      lo_memory->mi_stack->push( zcl_wasm_i32=>from_signed( 10 ) ).
      lo_memory->mi_stack->push( zcl_wasm_i32=>from_signed( 10 ) ).
      li_instruction->execute(
            EXPORTING
              io_memory  = lo_memory
              io_module  = lo_module
            CHANGING
              cs_control = ls_control ).
      lo_memory->mi_stack->pop( ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
