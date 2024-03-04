CLASS zcl_wasm_i64_rotr DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i64_rotr IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_rotr( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    DATA lv_hex TYPE x LENGTH 8.
    DATA lv_int TYPE int8.

    DATA(lv_bits) = io_memory->stack_pop_i64( )->get_signed( ) MOD 64.
    lv_hex = io_memory->stack_pop_i64( )->get_signed( ).

    DATA(lv_bytes) = lv_bits DIV 8.
    lv_bits = lv_bits MOD 8.

    IF lv_bytes > 0.
      SHIFT lv_hex RIGHT BY lv_bytes PLACES IN BYTE MODE CIRCULAR.
    ENDIF.

    IF lv_bits > 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |todo, execute instruction zcl_wasm_i64_rotr, shift { lv_bits } bits|.
    ENDIF.

    lv_int = lv_hex.
    io_memory->stack_push( zcl_wasm_i64=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
