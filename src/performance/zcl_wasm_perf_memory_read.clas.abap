CLASS zcl_wasm_perf_memory_read DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run RAISING zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_perf_memory_read IMPLEMENTATION.

  METHOD run.

    CONSTANTS lc_iterations TYPE i VALUE 200000.

* this is around 6mb of memory
    DATA(li_linear) = NEW zcl_wasm_memory_linear(
      iv_min = 100
      iv_max = 100 ).

    DO lc_iterations TIMES.
      li_linear->get(
        iv_length = 4
        iv_offset = 100000 ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
