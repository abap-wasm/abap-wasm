CLASS zcl_wasm_local_tee DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING
        !iv_localidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_localidx TYPE int8.
    CLASS-DATA gt_singletons TYPE STANDARD TABLE OF REF TO zcl_wasm_local_tee WITH DEFAULT KEY.

ENDCLASS.

CLASS zcl_wasm_local_tee IMPLEMENTATION.

  METHOD constructor.
    mv_localidx = iv_localidx.
  ENDMETHOD.

  METHOD class_constructor.
    DO 100 TIMES.
      DATA(lo_get) = NEW zcl_wasm_local_tee( CONV #( sy-index - 1 ) ).
      INSERT lo_get INTO TABLE gt_singletons.
    ENDDO.
  ENDMETHOD.

  METHOD parse.
    DATA lv_idx TYPE int8.
    lv_idx = io_body->shift_u32( ) + 1.
    READ TABLE gt_singletons INDEX lv_idx INTO ri_instruction.
    IF sy-subrc <> 0.
      ri_instruction = NEW zcl_wasm_local_tee( lv_idx ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-local-tee-x

    DATA(li_value) = io_memory->mi_stack->peek( ).

    io_memory->mi_frame->local_set(
      iv_index = mv_localidx
      ii_value = li_value ).

  ENDMETHOD.

ENDCLASS.
