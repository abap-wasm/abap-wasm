CLASS zcl_wasm_performance DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.

CLASS zcl_wasm_performance IMPLEMENTATION.

  METHOD run.

    TYPES: BEGIN OF ty_tests,
              description TYPE string,
              class       TYPE string,
            END OF ty_tests.

    DATA lt_tests TYPE STANDARD TABLE OF ty_tests WITH DEFAULT KEY.

    lt_tests = VALUE #(
      ( description = 'Memory read'  class = 'ZCL_WASM_PERF_MEMORY_READ' )
      ( description = 'Memory write' class = 'ZCL_WASM_PERF_MEMORY_WRITE' ) ).

    LOOP AT lt_tests INTO DATA(ls_test).
      GET RUN TIME FIELD DATA(lv_start).
      CALL METHOD (ls_test-class)=>run( ).
      GET RUN TIME FIELD DATA(lv_end).

      WRITE / |{ ls_test-description }: { lv_end - lv_start }ms|.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
