CLASS zcl_wasm_i32_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_value TYPE i.
  PRIVATE SECTION.
    DATA mv_value TYPE i.
ENDCLASS.

CLASS zcl_wasm_i32_const IMPLEMENTATION.

  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.
    zcl_wasm_i32=>const_(
      io_memory = io_memory
      iv_value  = mv_value ).
  ENDMETHOD.

ENDCLASS.