CLASS zcl_wasm_i32_mul DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_mul IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_mul( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    DATA lv_long1 TYPE int8.
    DATA lv_long2 TYPE int8.
    DATA lv_int4  TYPE i.
    DATA lv_res   TYPE int8.


    DATA(lo_val1) = io_memory->stack_pop_i32( ).
    DATA(lo_val2) = io_memory->stack_pop_i32( ).

* dont overflow, so convert to int8s
    lv_long1 = lo_val1->get_signed( ).
    lv_long2 = lo_val2->get_signed( ).
    lv_res = lv_long1 * lv_long2.

* connvert int8 to i
    lv_res = lv_res MOD 4294967296.
    IF lv_res > 2147483647.
      lv_res = lv_res - 4294967296.
    ENDIF.
    lv_int4 = lv_res.

    io_memory->stack_push( zcl_wasm_i32=>from_signed( lv_int4 ) ).

  ENDMETHOD.

ENDCLASS.
