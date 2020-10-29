CLASS zcl_wasm_memory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS stack_push
      IMPORTING
        !ii_value TYPE REF TO zif_wasm_value .
    METHODS stack_pop
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
    METHODS stack_peek
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
    METHODS stack_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS local_push
      IMPORTING
        !ii_value TYPE REF TO zif_wasm_value .
    METHODS local_get
      IMPORTING
        !iv_index       TYPE i
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
  PROTECTED SECTION.
    DATA mt_stack TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
    DATA mt_locals TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_MEMORY IMPLEMENTATION.


  METHOD local_get.

    DATA(lv_index) = iv_index + 1.
    READ TABLE mt_locals INDEX lv_index INTO ri_value.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD local_push.

    APPEND ii_value TO mt_locals.

  ENDMETHOD.


  METHOD stack_length.

    rv_length = lines( mt_stack ).

  ENDMETHOD.


  METHOD stack_peek.

    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.

  ENDMETHOD.


  METHOD stack_pop.

    ASSERT lines( mt_stack ) > 0.

    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.
    DELETE mt_stack INDEX lv_last.

  ENDMETHOD.


  METHOD stack_push.

    APPEND ii_value TO mt_stack.

  ENDMETHOD.
ENDCLASS.
