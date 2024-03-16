CLASS zcl_wasm_performance DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING
        VALUE(rv_json) TYPE string.
ENDCLASS.

CLASS zcl_wasm_performance IMPLEMENTATION.

  METHOD run.

    TYPES: BEGIN OF ty_tests,
              description TYPE string,
              class       TYPE string,
            END OF ty_tests.
    TYPES: BEGIN OF ty_result,
             description TYPE string,
             time        TYPE i,
           END OF ty_result.
    DATA lt_tests TYPE STANDARD TABLE OF ty_tests WITH DEFAULT KEY.
    DATA lt_results TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    lt_tests = VALUE #(
      ( description = 'Memory read'  class = 'ZCL_WASM_PERF_MEMORY_READ' )
      ( description = 'Memory write' class = 'ZCL_WASM_PERF_MEMORY_WRITE' )
      ( description = 'i32.rotl'     class = 'ZCL_WASM_PERF_I32_ROTL' )
      ( description = 'i32.load'     class = 'ZCL_WASM_PERF_I32_LOAD' )
      ( description = 'i32.store'    class = 'ZCL_WASM_PERF_I32_STORE' ) ).

    LOOP AT lt_tests INTO DATA(ls_test).
      GET RUN TIME FIELD DATA(lv_start).
      CALL METHOD (ls_test-class)=>run.
      GET RUN TIME FIELD DATA(lv_end).

      WRITE / |{ ls_test-description }: { lv_end - lv_start }ms|.
      APPEND VALUE ty_result( description = ls_test-description time = lv_end - lv_start ) TO lt_results.
    ENDLOOP.

    rv_json = /ui2/cl_json=>serialize( lt_results ).

  ENDMETHOD.

ENDCLASS.
