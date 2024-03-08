CLASS zcl_wasm_memory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_alignment_8bit TYPE int8 VALUE 0.
    CONSTANTS c_alignment_16bit TYPE int8 VALUE 1.
    CONSTANTS c_alignment_32bit TYPE int8 VALUE 2.
    CONSTANTS c_alignment_64bit TYPE int8 VALUE 3.

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
    METHODS stack_pop_i64
      RETURNING
        VALUE(ro_value) TYPE REF TO zcl_wasm_i64
      RAISING zcx_wasm.
    METHODS stack_peek
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value .
    METHODS stack_length
      RETURNING
        VALUE(rv_length) TYPE i .

*********** Frames with locals
    METHODS push_frame.
    METHODS get_frame
      RETURNING
        VALUE(ri_frame) TYPE REF TO zif_wasm_memory_frame
      RAISING zcx_wasm.
    METHODS pop_frame
      RAISING zcx_wasm.

*********** GLOBAL
    METHODS global_get
      IMPORTING
        iv_index TYPE int8
      RETURNING
        VALUE(rv_value) TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.
    METHODS global_set
      IMPORTING
        iv_index TYPE int8
        ii_value TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.
    METHODS global_append
      IMPORTING
        ii_value TYPE REF TO zif_wasm_value
      RETURNING
        VALUE(rv_index) TYPE i.

*********** DEFAULT LINEAR
    METHODS get_linear
      RETURNING
        VALUE(ri_linear) TYPE REF TO zif_wasm_memory_linear
      RAISING
        zcx_wasm.

    METHODS set_linear
      IMPORTING
        ii_linear TYPE REF TO zif_wasm_memory_linear.

    METHODS has_linear
      RETURNING
        VALUE(rv_exists) TYPE abap_bool.

  PROTECTED SECTION.
    DATA mt_stack  TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
    DATA mi_linear TYPE REF TO zif_wasm_memory_linear.
    DATA mt_frames TYPE STANDARD TABLE OF REF TO zif_wasm_memory_frame WITH DEFAULT KEY.
    DATA mt_globals TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH EMPTY KEY.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_memory IMPLEMENTATION.

  METHOD global_get.
    READ TABLE mt_globals INDEX iv_index + 1 INTO rv_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |zcl_wasm_memory: global_get, not found, index { iv_index }|.
    ENDIF.
  ENDMETHOD.

  METHOD global_set.
* todo: raise error if the set changes the type of the global?
    IF lines( mt_globals ) < iv_index + 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: global_set, not found'.
    ENDIF.
    mt_globals[ iv_index + 1 ] = ii_value.
  ENDMETHOD.

  METHOD global_append.
    APPEND ii_value TO mt_globals.
    rv_index = lines( mt_globals ) - 1.
  ENDMETHOD.

  METHOD push_frame.
    DATA(lo_frame) = NEW zcl_wasm_memory_frame( ).
    APPEND lo_frame TO mt_frames.
  ENDMETHOD.

  METHOD pop_frame.
    IF lines( mt_frames ) = 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: no frames, pop'.
    ENDIF.
    DELETE mt_frames INDEX lines( mt_frames ).
  ENDMETHOD.

  METHOD get_frame.
    DATA(lv_last) = lines( mt_frames ).
    READ TABLE mt_frames INDEX lv_last INTO ri_frame.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: no frames, get'.
    ENDIF.
  ENDMETHOD.

  METHOD get_linear.
    IF mi_linear IS INITIAL.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: no linear memory'.
    ENDIF.

    ri_linear = mi_linear.
  ENDMETHOD.

  METHOD has_linear.
    rv_exists = xsdbool( mi_linear IS NOT INITIAL ).
  ENDMETHOD.

  METHOD set_linear.
    mi_linear = ii_linear.
  ENDMETHOD.

  METHOD stack_length.
    rv_length = lines( mt_stack ).
  ENDMETHOD.

  METHOD stack_peek.
    DATA(lv_last) = lines( mt_stack ).
    READ TABLE mt_stack INDEX lv_last INTO ri_value.
  ENDMETHOD.


  METHOD stack_pop.

    DATA(lv_length) = lines( mt_stack ).
    IF lv_length = 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: nothing to pop'.
    ENDIF.

    READ TABLE mt_stack INDEX lv_length INTO ri_value.
    DELETE mt_stack INDEX lv_length.

    " IF ri_value IS INITIAL.
    "   RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: stack popped initial value'.
    " ENDIF.

  ENDMETHOD.


  METHOD stack_pop_i64.

    DATA(li_pop) = stack_pop( ).

    IF li_pop->get_type( ) <> zcl_wasm_types=>c_value_type-i64.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: pop, expected i64'.
    ENDIF.

    ro_value ?= li_pop.

  ENDMETHOD.

  METHOD stack_pop_i32.

    DATA(li_pop) = stack_pop( ).

    IF li_pop->get_type( ) <> zcl_wasm_types=>c_value_type-i32.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: pop, expected i32'.
    ENDIF.

    ro_value ?= li_pop.

  ENDMETHOD.


  METHOD stack_push.

    APPEND ii_value TO mt_stack.

  ENDMETHOD.
ENDCLASS.
