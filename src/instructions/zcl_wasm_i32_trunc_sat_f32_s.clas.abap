CLASS zcl_wasm_i32_trunc_sat_f32_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_trunc_sat_f32_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_trunc_sat_f32_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'todo, execute instruction zcl_wasm_i32_trunc_sat_f32_s'.
  ENDMETHOD.

ENDCLASS.
