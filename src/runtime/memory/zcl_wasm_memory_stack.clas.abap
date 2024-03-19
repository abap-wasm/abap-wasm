CLASS zcl_wasm_memory_stack DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_memory_stack.
  PRIVATE SECTION.
    DATA mt_stack TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH DEFAULT KEY.
ENDCLASS.

CLASS zcl_wasm_memory_stack IMPLEMENTATION.

  METHOD zif_wasm_memory_stack~get_length.
    rv_length = lines( mt_stack ).
  ENDMETHOD.

  METHOD zif_wasm_memory_stack~pop.

    DATA(lv_length) = lines( mt_stack ).
    "##feature-start=debug
    IF lv_length = 0.
      RAISE EXCEPTION TYPE zcx_wasm
        EXPORTING
          text = |zcl_wasm_memory: nothing to pop|.
    ENDIF.
    "##feature-end=debug

    READ TABLE mt_stack INDEX lv_length INTO ri_value.
    DELETE mt_stack INDEX lv_length.

  ENDMETHOD.

  METHOD zif_wasm_memory_stack~peek.

    DATA(lv_length) = lines( mt_stack ).
    "##feature-start=debug
    ASSERT lv_length > 0.
    "##feature-end=debug

    READ TABLE mt_stack INDEX lv_length INTO ri_value.

  ENDMETHOD.


  METHOD zif_wasm_memory_stack~pop_i64.

    DATA(li_pop) = zif_wasm_memory_stack~pop( ).

    "##feature-start=debug
    IF li_pop->get_type( ) <> zif_wasm_types=>c_value_type-i64.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: pop, expected i64'.
    ENDIF.
    "##feature-end=debug

    ro_value ?= li_pop.

  ENDMETHOD.

  METHOD zif_wasm_memory_stack~pop_i32.

    DATA(li_pop) = zif_wasm_memory_stack~pop( ).

    "##feature-start=debug
    IF li_pop->get_type( ) <> zif_wasm_types=>c_value_type-i32.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'zcl_wasm_memory: pop, expected i32'.
    ENDIF.
    "##feature-end=debug

    ro_value ?= li_pop.

  ENDMETHOD.


  METHOD zif_wasm_memory_stack~push.

    APPEND ii_value TO mt_stack.

  ENDMETHOD.

ENDCLASS.
