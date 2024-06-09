CLASS zcl_wasm_i32_shl DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.

CLASS zcl_wasm_i32_shl IMPLEMENTATION.

  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i32_shl( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
* https://webassembly.github.io/spec/core/exec/numerics.html#xref-exec-numerics-op-ishl-mathrm-ishl-n-i-1-i-2

    DATA lv_hex TYPE x LENGTH 4.
    DATA lv_int TYPE i.

    DATA(lv_bits) = CAST zcl_wasm_i32( io_memory->mi_stack->pop( ) )->mv_value MOD 32.
    lv_hex = CAST zcl_wasm_i32( io_memory->mi_stack->pop( ) )->mv_value.

    DATA(lv_bytes) = lv_bits DIV 8.
    lv_bits = lv_bits MOD 8.

    IF lv_bytes > 0.
      SHIFT lv_hex LEFT BY lv_bytes PLACES IN BYTE MODE.
    ENDIF.

    DO lv_bits TIMES.
      DO 31 TIMES.
        DATA(lv_offset) = sy-index + 1.
        GET BIT lv_offset OF lv_hex INTO DATA(lv_set).
        lv_offset = lv_offset - 1.
        SET BIT lv_offset OF lv_hex TO lv_set.
      ENDDO.
      SET BIT 32 OF lv_hex TO 0.
    ENDDO.

    lv_int = lv_hex.
    io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( lv_int ) ).
  ENDMETHOD.

ENDCLASS.
