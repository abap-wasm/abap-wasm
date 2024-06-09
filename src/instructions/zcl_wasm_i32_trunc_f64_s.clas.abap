CLASS zcl_wasm_i32_trunc_f64_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_trunc_f64_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_trunc_f64_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* f64 to i32
    DATA lv_int TYPE i.

    DATA(lo_f64) = CAST zcl_wasm_f64( io_memory->mi_stack->pop( ) ).
    IF lo_f64->get_special( ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = 'i32.trunc_f64_s: todo special value'.
    ENDIF.

    DATA(lv_float) = lo_f64->get_value( ).
    lv_int = lv_float. " todo, this will overflow/break/fail?
    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
