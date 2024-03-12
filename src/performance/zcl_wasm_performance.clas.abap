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
      ( description = 'Memory read'  class = 'zcl_wasm_perf_memory_read' )
      ( description = 'Memory write' class = 'zcl_wasm_perf_memory_write' ) ).

    LOOP AT lt_tests INTO DATA(ls_test).
      GET RUN TIME FIELD DATA(lv_start).
      CALL METHOD (ls_test-class)=>run( ).
      GET RUN TIME FIELD DATA(lv_end).

      WRITE / |{ ls_test-description } took { lv_end - lv_start } ms|.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
