CLASS zcl_wasm_i64_extend32_s DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_extend32_s IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_extend32_s( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

    DATA lv_int TYPE int8.
    DATA lv_hex TYPE x LENGTH 8.

    DATA(li_value) = io_memory->mi_stack->pop( ).
    "##feature-start=debug
    IF li_value->get_type( ) <> zif_wasm_types=>c_value_type-i64.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_i64_extend32_s: expected i64, got { li_value->get_type( ) }|.
    ENDIF.
    "##feature-end=debug
    lv_hex = CAST zcl_wasm_i64( li_value )->get_signed( ).

    GET BIT 33 OF lv_hex INTO DATA(lv_sign).
    IF lv_sign = 1.
      lv_hex(4) = 'FFFFFFFF'.
    ELSE.
      lv_hex(4) = '00000000'.
    ENDIF.

    lv_int = lv_hex.
    io_memory->mi_stack->push( zcl_wasm_i64=>from_signed( lv_int ) ).

  ENDMETHOD.

ENDCLASS.
