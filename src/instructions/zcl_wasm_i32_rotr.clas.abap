CLASS zcl_wasm_i32_rotr DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_rotr IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_rotr( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-irotr-mathrm-irotr-n-i-1-i-2

    DATA lv_hex TYPE x LENGTH 4.
    DATA lv_int TYPE i.

    DATA(lv_bits) = io_memory->mi_stack->pop_i32( )->mv_value MOD 32.
    lv_hex = io_memory->mi_stack->pop_i32( )->mv_value.

    DATA(lv_bytes) = lv_bits DIV 8.
    lv_bits = lv_bits MOD 8.

    IF lv_bytes > 0.
      SHIFT lv_hex RIGHT BY lv_bytes PLACES IN BYTE MODE CIRCULAR.
    ENDIF.

    DO lv_bits TIMES.
      GET BIT 32 OF lv_hex INTO DATA(lv_set).
      DO 32 TIMES.
        DATA(lv_offset) = sy-index.
        GET BIT lv_offset OF lv_hex INTO DATA(lv_get).
        SET BIT lv_offset OF lv_hex TO lv_set.
        lv_set = lv_get.
      ENDDO.
    ENDDO.

    lv_int = lv_hex.
    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).

  ENDMETHOD.

ENDCLASS.
