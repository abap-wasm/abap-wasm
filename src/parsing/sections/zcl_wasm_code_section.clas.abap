CLASS zcl_wasm_code_section DEFINITION PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zcl_wasm_module=>ty_codes
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_code_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-codesec

    DATA lt_locals TYPE zcl_wasm_module=>ty_locals.
    DATA lt_instructions TYPE zif_wasm_instruction=>ty_list.

    DO io_body->shift_u32( ) TIMES.

      DATA(lv_code_size) = io_body->shift_u32( ).

      DATA(lo_code) = NEW zcl_wasm_binary_stream( io_body->shift( lv_code_size ) ).

      CLEAR lt_locals.
      DATA(lv_locals_count) = lo_code->shift_u32( ).
      DO lv_locals_count TIMES.
        DATA(lv_count) = lo_code->shift_u32( ).
        DATA(lv_locals_type) = lo_code->shift( 1 ).
        APPEND VALUE #(
          count = lv_count
          type  = lv_locals_type ) TO lt_locals.
      ENDDO.

      CLEAR lt_instructions.

      zcl_wasm_instructions=>parse(
        EXPORTING io_body         = lo_code
        IMPORTING et_instructions = lt_instructions ).

      APPEND VALUE #(
        instructions = lt_instructions
        locals       = lt_locals ) TO rt_results.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
