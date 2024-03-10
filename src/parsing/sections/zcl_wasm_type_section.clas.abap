CLASS zcl_wasm_type_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zcl_wasm_module=>ty_types
      RAISING
        zcx_wasm.
ENDCLASS.

CLASS zcl_wasm_type_section IMPLEMENTATION.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#type-section

    DO io_body->shift_u32( ) TIMES.
      IF io_body->shift( 1 ) <> zif_wasm_types=>c_function_type.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse type section, expected function type|.
      ENDIF.

      APPEND VALUE #(
            parameter_types = io_body->shift( io_body->shift_u32( ) )
            result_types    = io_body->shift( io_body->shift_u32( ) )
            ) TO rt_results.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
