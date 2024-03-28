CLASS zcl_wasm_memory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_alignment_8bit TYPE int8 VALUE 0.
    CONSTANTS c_alignment_16bit TYPE int8 VALUE 1.
    CONSTANTS c_alignment_32bit TYPE int8 VALUE 2.
    CONSTANTS c_alignment_64bit TYPE int8 VALUE 3.

    METHODS constructor.

*********** STACK
    DATA mi_stack TYPE REF TO zif_wasm_memory_stack.

*********** LOCALS
    TYPES ty_locals TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
    DATA mt_locals TYPE ty_locals.

    METHODS push_locals.

    METHODS pop_locals
      RAISING zcx_wasm.

*********** GLOBAL
    METHODS get_globals
      RETURNING
        VALUE(ri_globals) TYPE REF TO zif_wasm_memory_globals
      RAISING
        zcx_wasm.

*********** DEFAULT LINEAR
    DATA mi_linear TYPE REF TO zcl_wasm_memory_linear.

************* TABLES
    METHODS table_add
      IMPORTING
        is_table TYPE zcl_wasm_table_section=>ty_table
      RAISING
        zcx_wasm.

    METHODS table_set
      IMPORTING
        iv_tableidx TYPE i
        iv_offset   TYPE i
        ii_value    TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.

    METHODS table_size
      IMPORTING
        iv_tableidx    TYPE i
      RETURNING
        VALUE(rv_size) TYPE i
      RAISING
        zcx_wasm.

    METHODS table_get
      IMPORTING
        iv_tableidx     TYPE i
        iv_offset       TYPE i
      RETURNING
        VALUE(ri_value) TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.

    METHODS table_grow
      IMPORTING
        iv_tableidx TYPE i
        iv_count    TYPE i
        ii_value    TYPE REF TO zif_wasm_value
      RAISING
        zcx_wasm.

    METHODS table_get_max
      IMPORTING
        iv_tableidx   TYPE i
      RETURNING
        VALUE(rv_max) TYPE i
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
    DATA mi_globals TYPE REF TO zif_wasm_memory_globals.

    DATA mt_stack  TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
    DATA mt_frames TYPE STANDARD TABLE OF ty_locals WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_table,
             type     TYPE zcl_wasm_table_section=>ty_table,
             contents TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH EMPTY KEY,
           END OF ty_table.
    DATA mt_tables TYPE STANDARD TABLE OF ty_table WITH EMPTY KEY.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_memory IMPLEMENTATION.

  METHOD constructor.
    mi_globals = NEW zcl_wasm_memory_globals( ).
    mi_stack = NEW zcl_wasm_memory_stack( ).
  ENDMETHOD.

  METHOD get_globals.
    ri_globals = mi_globals.
  ENDMETHOD.

  METHOD table_get.
    DATA(lv_idx) = iv_tableidx + 1.
    READ TABLE mt_tables INDEX lv_idx ASSIGNING FIELD-SYMBOL(<lt_table>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_get, not found, index { iv_tableidx }|.
    ENDIF.
    DATA(lv_offset) = iv_offset + 1.
    IF lv_offset > lines( <lt_table>-contents ).
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_get, out of bounds|.
    ENDIF.
    ri_value = <lt_table>-contents[ lv_offset ].
  ENDMETHOD.

  METHOD table_size.
    DATA(lv_idx) = iv_tableidx + 1.
    READ TABLE mt_tables INDEX lv_idx ASSIGNING FIELD-SYMBOL(<lt_table>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_size, not found, index { iv_tableidx }|.
    ENDIF.
    rv_size = lines( <lt_table>-contents ).
  ENDMETHOD.

  METHOD table_set.
*    WRITE / |table set { iv_tableidx }, { iv_offset }|.
    DATA(lv_idx) = iv_tableidx + 1.
    READ TABLE mt_tables INDEX lv_idx ASSIGNING FIELD-SYMBOL(<lt_table>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_set, not found, index { iv_tableidx }|.
    ENDIF.
    DATA(lv_offset) = iv_offset + 1.
    IF lv_offset > lines( <lt_table>-contents ).
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_get, out of bounds|.
    ENDIF.
    <lt_table>-contents[ lv_offset ] = ii_value.
  ENDMETHOD.

  METHOD table_add.
    DATA ls_table TYPE ty_table.
    DATA li_val   TYPE REF TO zif_wasm_value.

    ls_table-type = is_table.
    INSERT ls_table INTO TABLE mt_tables.

    CASE ls_table-type-reftype.
      WHEN zif_wasm_types=>c_reftype-funcref.
        li_val = NEW zcl_wasm_funcref( -1 ).
      WHEN zif_wasm_types=>c_reftype-externref.
        li_val = NEW zcl_wasm_externref( -1 ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wasm
          EXPORTING
            text = |zcl_wasm_memory: table_grow, unknown reftype { ls_table-type-reftype }|.
    ENDCASE.

    table_grow(
      iv_tableidx = lines( mt_tables ) - 1
      iv_count    = is_table-limit-min
      ii_value    = li_val ).
  ENDMETHOD.

  METHOD table_get_max.

    DATA(lv_idx) = iv_tableidx + 1.
    READ TABLE mt_tables INDEX lv_idx ASSIGNING FIELD-SYMBOL(<lt_table>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_set, not found, index { iv_tableidx }|.
    ENDIF.

    rv_max = <lt_table>-type-limit-max.

  ENDMETHOD.

  METHOD table_grow.

    DATA(lv_idx) = iv_tableidx + 1.
    READ TABLE mt_tables INDEX lv_idx ASSIGNING FIELD-SYMBOL(<lt_table>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: table_set, not found, index { iv_tableidx }|.
    ENDIF.

* todo, validate ii_value is of the expected type?
    DO iv_count TIMES.
      INSERT ii_value INTO TABLE <lt_table>-contents.
    ENDDO.

  ENDMETHOD.

  METHOD push_locals.
    APPEND mt_locals TO mt_frames.
    CLEAR mt_locals.
  ENDMETHOD.

  METHOD pop_locals.
    DATA lv_last TYPE i.
    lv_last = lines( mt_frames ).
    "##feature-start=debug
    IF lv_last = 0.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: no frames, pop'.
    ENDIF.
    "##feature-end=debug

    READ TABLE mt_frames INTO mt_locals INDEX lv_last.
    DELETE mt_frames INDEX lv_last.
  ENDMETHOD.

ENDCLASS.
