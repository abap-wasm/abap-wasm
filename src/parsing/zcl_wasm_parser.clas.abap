CLASS zcl_wasm_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !iv_wasm         TYPE xstring
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module
      RAISING
        zcx_wasm.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_parser IMPLEMENTATION.

  METHOD parse.

    CONSTANTS lc_magic   TYPE x LENGTH 4 VALUE '0061736D'.
    CONSTANTS lc_version TYPE x LENGTH 4 VALUE '01000000'.

    DATA lt_functions TYPE zcl_wasm_module=>ty_functions.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_wasm ).

* https://webassembly.github.io/spec/core/binary/modules.html#binary-module
    IF lo_stream->shift( 4 ) <> lc_magic.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |unexpected magic number|.
    ENDIF.
    IF lo_stream->shift( 4 ) <> lc_version.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |unexpected version|.
    ENDIF.

    WHILE lo_stream->get_length( ) > 0.
* https://webassembly.github.io/spec/core/binary/modules.html#sections
      DATA(lv_section) = lo_stream->shift( 1 ).
      DATA(lv_length) = lo_stream->shift_u32( ).
      DATA(lo_body) = NEW zcl_wasm_binary_stream( lo_stream->shift( lv_length ) ).

      CASE lv_section.
        WHEN zif_wasm_sections=>gc_section_custom.
          zcl_wasm_custom_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_type.
          DATA(lt_types) = zcl_wasm_type_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_import.
          zcl_wasm_import_section=>parse(
            EXPORTING
              io_body           = lo_body
            IMPORTING
              eo_import_section = DATA(lo_import_section)
            CHANGING
              ct_functions      = lt_functions ).
        WHEN zif_wasm_sections=>gc_section_function.
          zcl_wasm_function_section=>parse(
            EXPORTING
              io_body      = lo_body
            CHANGING
              ct_functions = lt_functions ).
        WHEN zif_wasm_sections=>gc_section_table.
          DATA(lo_table_section) = zcl_wasm_table_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_memory.
          DATA(lo_memory_section) = zcl_wasm_memory_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_global.
          DATA(lo_global_section) = zcl_wasm_global_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_export.
          DATA(lt_exports) = zcl_wasm_export_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_start.
* https://webassembly.github.io/spec/core/binary/modules.html#start-section
* todo
          DATA(lv_funcidx) = lo_body->shift_u32( ).
        WHEN zif_wasm_sections=>gc_section_element.
          DATA(lo_element_section) = zcl_wasm_element_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_code.
          DATA(lt_codes) = zcl_wasm_code_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_data.
          DATA(lo_data_section) = zcl_wasm_data_section=>parse( lo_body ).
        WHEN zif_wasm_sections=>gc_section_data_count.
          DATA(lv_data_count) = lo_body->shift_u32( ).
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |unknown section: { lv_section }|.
      ENDCASE.
    ENDWHILE.

    ro_module = NEW #(
      it_types           = lt_types
      it_codes           = lt_codes
      it_exports         = lt_exports
      io_data_section    = lo_data_section
      io_memory_section  = lo_memory_section
      io_global_section  = lo_global_section
      io_import_section  = lo_import_section
      io_table_section   = lo_table_section
      io_element_section = lo_element_section
      it_functions       = lt_functions ).

  ENDMETHOD.

ENDCLASS.
