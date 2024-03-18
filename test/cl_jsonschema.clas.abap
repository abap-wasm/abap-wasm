CLASS cl_jsonschema DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_hex         TYPE xstring
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS cl_jsonschema IMPLEMENTATION.

  METHOD run.

    GET RUN TIME FIELD DATA(lv_start).
    DATA(li_wasm) = zcl_wasm=>create_with_wasm( iv_hex ).
    GET RUN TIME FIELD DATA(lv_end).

    DATA(lv_parsing) = lv_end - lv_start.
    WRITE / |{ lv_parsing }ms parsing jsonschema|.

    rv_json = '{"parsing": "' && lv_parsing && '"}'.

  ENDMETHOD.

ENDCLASS.
