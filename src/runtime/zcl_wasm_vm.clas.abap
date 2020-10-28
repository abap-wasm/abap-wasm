CLASS zcl_wasm_vm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory.

    METHODS execute
      IMPORTING
        iv_instructions TYPE xstring.
  PROTECTED SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_VM IMPLEMENTATION.


  METHOD constructor.
    mo_memory = io_memory.
  ENDMETHOD.


  METHOD execute.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_instructions ).

    DATA(lv_peek) = lo_stream->peek( 1 ).

    CASE lv_peek.
      WHEN zcl_wasm_instructions=>c_instructions-i32_add.
        zcl_wasm_i32=>add( mo_memory ).
      WHEN zcl_wasm_instructions=>c_instructions-i32_sub.
*        zcl_wasm_i32=>sub( mo_memory ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
