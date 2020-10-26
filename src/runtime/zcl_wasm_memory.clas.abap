CLASS zcl_wasm_memory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS push
      IMPORTING
        !ii_value TYPE REF TO zif_wasm_value .
    METHODS pop
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
    METHODS peek
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
    METHODS get_length
      RETURNING
        VALUE(rv_length) TYPE i .
  PROTECTED SECTION.
    DATA mt_stack TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_MEMORY IMPLEMENTATION.


  METHOD get_length.

    rv_length = lines( mt_stack ).

  ENDMETHOD.


  METHOD peek.

    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.

  ENDMETHOD.


  METHOD pop.

    ASSERT lines( mt_stack ) > 0.

    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.
    DELETE mt_stack INDEX lv_last.

  ENDMETHOD.


  METHOD push.

    APPEND ii_value TO mt_stack.

  ENDMETHOD.
ENDCLASS.
