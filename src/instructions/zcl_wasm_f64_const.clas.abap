CLASS zcl_wasm_f64_const DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !io_float TYPE REF TO zcl_wasm_f64.

    CLASS-METHODS parse
      IMPORTING
        !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_value TYPE REF TO zcl_wasm_f64.
ENDCLASS.



CLASS ZCL_WASM_F64_CONST IMPLEMENTATION.


  METHOD constructor.
    mo_value = io_float.
  ENDMETHOD.


  METHOD parse.
    ri_instruction = NEW zcl_wasm_f64_const( io_body->shift_f64( ) ).
  ENDMETHOD.


  METHOD zif_wasm_instruction~execute.
    io_memory->mi_stack->push( mo_value ).
  ENDMETHOD.
ENDCLASS.
