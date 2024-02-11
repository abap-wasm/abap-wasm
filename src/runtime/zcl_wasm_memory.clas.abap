CLASS zcl_wasm_memory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

*********** STACK
    METHODS stack_push
      IMPORTING
        !ii_value TYPE REF TO zif_wasm_value .
    METHODS stack_pop
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.
    METHODS stack_pop_i32
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i32
      RAISING zcx_wasm.
    METHODS stack_peek
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
    METHODS stack_length
      RETURNING
        VALUE(rv_length) TYPE i .

*********** LOCAL
    METHODS local_push
      IMPORTING
        !ii_value TYPE REF TO zif_wasm_value .
    METHODS local_get
      IMPORTING
        !iv_index       TYPE int8
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.
    METHODS local_set
      IMPORTING
        iv_index TYPE int8
        ii_value TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.

*********** GLOBAL
* todo

*********** DEFAULT LINEAR
    METHODS linear_set
      IMPORTING
        iv_offset TYPE int8
        iv_bytes  TYPE xstring.

    METHODS linear_get
      IMPORTING
        iv_length TYPE int8
        iv_offset TYPE int8
      RETURNING
        VALUE(rv_bytes) TYPE xstring
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
    DATA mt_stack  TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
    DATA mt_locals TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
    DATA mv_linear TYPE xstring.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_memory IMPLEMENTATION.

  METHOD linear_set.
    mv_linear = iv_bytes.
  ENDMETHOD.

  METHOD linear_get.
    rv_bytes = mv_linear+iv_offset(iv_length).
  ENDMETHOD.

  METHOD local_get.

    DATA(lv_index) = iv_index + 1.
    READ TABLE mt_locals INDEX lv_index INTO ri_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: not found in local memory, local_get' ).
    ENDIF.

  ENDMETHOD.


  METHOD local_push.

* note: locals are popped from the stack in reverse order
    INSERT ii_value INTO mt_locals INDEX 1.

  ENDMETHOD.


  METHOD local_set.

    DATA(lv_index) = iv_index + 1.
    MODIFY mt_locals INDEX lv_index FROM ii_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: not found in local memory, local_set' ).
    ENDIF.

  ENDMETHOD.


  METHOD stack_length.

    rv_length = lines( mt_stack ).

  ENDMETHOD.


  METHOD stack_peek.

    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.

  ENDMETHOD.


  METHOD stack_pop.

    IF lines( mt_stack ) = 0.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: nothing to pop' ).
    ENDIF.

    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.
    DELETE mt_stack INDEX lv_last.

  ENDMETHOD.


  METHOD stack_pop_i32.

    DATA(li_pop) = stack_pop( ).

    IF li_pop->get_type( ) <> zcl_wasm_types=>c_value_type-i32.
      RAISE EXCEPTION NEW zcx_wasm( text = 'zcl_wasm_memory: pop, expected i32' ).
    ENDIF.

    ro_value ?= li_pop.

  ENDMETHOD.


  METHOD stack_push.

    APPEND ii_value TO mt_stack.

  ENDMETHOD.
ENDCLASS.
