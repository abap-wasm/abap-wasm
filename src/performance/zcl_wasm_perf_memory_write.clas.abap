CLASS zcl_wasm_perf_memory_write DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run RAISING zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_perf_memory_write IMPLEMENTATION.

  METHOD run.

    CONSTANTS lc_iterations TYPE i VALUE 1000.
    DATA lv_bytes1 TYPE x LENGTH 4.
    DATA lv_bytes2 TYPE x LENGTH 4.

* this is around 6mb of memory
    DATA(li_linear) = NEW zcl_wasm_memory_linear(
      iv_min = 100
      iv_max = 100 ).

    lv_bytes1 = '11111111'.
    lv_bytes2 = '22222222'.

    DO lc_iterations TIMES.
      IF sy-index MOD 2 = 0.
        li_linear->set(
          iv_offset = 100000
          iv_bytes  = lv_bytes1 ).
      ELSE.
        li_linear->set(
          iv_offset = 100000
          iv_bytes  = lv_bytes2 ).
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
