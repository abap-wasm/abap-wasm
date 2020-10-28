CLASS zcl_wasm_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !iv_wasm         TYPE xstring
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module .
  PROTECTED SECTION.
* Note that these constants are not structured as they contain JS keywords
    CONSTANTS:
      gc_section_custom   TYPE x LENGTH 1 VALUE '00',
      gc_section_type     TYPE x LENGTH 1 VALUE '01',
      gc_section_import   TYPE x LENGTH 1 VALUE '02',
      gc_section_function TYPE x LENGTH 1 VALUE '03',
      gc_section_table    TYPE x LENGTH 1 VALUE '04',
      gc_section_memory   TYPE x LENGTH 1 VALUE '05',
      gc_section_global   TYPE x LENGTH 1 VALUE '06',
      gc_section_export   TYPE x LENGTH 1 VALUE '07',
      gc_section_start    TYPE x LENGTH 1 VALUE '08',
      gc_section_element  TYPE x LENGTH 1 VALUE '09',
      gc_section_code     TYPE x LENGTH 1 VALUE '10',
      gc_section_data     TYPE x LENGTH 1 VALUE '11'.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_PARSER IMPLEMENTATION.


  METHOD parse.

    CONSTANTS lc_magic TYPE x LENGTH 4 VALUE '0061736D'.
    CONSTANTS lc_version TYPE x LENGTH 4 VALUE '01000000'.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_wasm ).

* https://webassembly.github.io/spec/core/binary/modules.html#binary-module
    ASSERT lo_stream->shift( 4 ) = lc_magic.
    ASSERT lo_stream->shift( 4 ) = lc_version.

*    WHILE lo_stream->get_length( ) > 0.
* https://webassembly.github.io/spec/core/binary/modules.html#sections
      DATA(lv_section) = lo_stream->shift( 1 ).
      DATA(lv_length) = lo_stream->shift( 1 ).

      CASE lv_section.
        WHEN gc_section_custom.
        WHEN gc_section_type.
        WHEN gc_section_import.
        WHEN gc_section_function.
        WHEN gc_section_table.
        WHEN gc_section_memory.
        WHEN gc_section_global.
        WHEN gc_section_export.
        WHEN gc_section_start.
        WHEN gc_section_element.
        WHEN gc_section_code.
        WHEN gc_section_data.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
*    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
