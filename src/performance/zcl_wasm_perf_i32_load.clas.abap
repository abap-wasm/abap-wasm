CLASS zcl_wasm_perf_i32_load DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run RAISING zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_perf_i32_load IMPLEMENTATION.

  METHOD run.

    CONSTANTS lc_iterations TYPE i VALUE 160000.

    DATA lo_module TYPE REF TO zcl_wasm_module.
    DATA ls_control TYPE zif_wasm_instruction=>ty_control.

    DATA(li_instruction) = CAST zif_wasm_instruction( NEW zcl_wasm_i32_load(
      iv_align  = zcl_wasm_memory=>c_alignment_32bit
      iv_offset = 32 ) ).

    DATA(lo_memory) = NEW zcl_wasm_memory( ).
    lo_memory->mi_linear = NEW zcl_wasm_memory_linear(
      iv_min = 1
      iv_max = 1 ).

    DO lc_iterations TIMES.
      lo_memory->mi_stack->push( zcl_wasm_i32=>gc_zero ).
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
