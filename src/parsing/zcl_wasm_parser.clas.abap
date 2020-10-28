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
      gc_section_code     TYPE x LENGTH 1 VALUE '0A',
      gc_section_data     TYPE x LENGTH 1 VALUE '0B'.

    METHODS:
      parse_type
        IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream,
      parse_function
        IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream,
      parse_export
        IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream,
      parse_code
        IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream,
      parse_custom
        IMPORTING io_body TYPE REF TO zcl_wasm_binary_stream.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_PARSER IMPLEMENTATION.


  METHOD parse.

    DATA lv_hex TYPE x LENGTH 1.

    CONSTANTS lc_magic TYPE x LENGTH 4 VALUE '0061736D'.
    CONSTANTS lc_version TYPE x LENGTH 4 VALUE '01000000'.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_wasm ).

* https://webassembly.github.io/spec/core/binary/modules.html#binary-module
    ASSERT lo_stream->shift( 4 ) = lc_magic.
    ASSERT lo_stream->shift( 4 ) = lc_version.

    WHILE lo_stream->get_length( ) > 0.
* https://webassembly.github.io/spec/core/binary/modules.html#sections
      DATA(lv_section) = lo_stream->shift( 1 ).
      lv_hex = lo_stream->shift( 1 ).
      DATA(lv_length) = CONV i( lv_hex ).
      DATA(lo_body) = NEW zcl_wasm_binary_stream( lo_stream->shift( lv_length ) ).

      CASE lv_section.
        WHEN gc_section_custom.
          parse_custom( lo_body ).
        WHEN gc_section_type.
          parse_type( lo_body ).
        WHEN gc_section_import.
          ASSERT 0 = 'todo'.
        WHEN gc_section_function.
          parse_function( lo_body ).
        WHEN gc_section_table.
          ASSERT 0 = 'todo'.
        WHEN gc_section_memory.
          ASSERT 0 = 'todo'.
        WHEN gc_section_global.
          ASSERT 0 = 'todo'.
        WHEN gc_section_export.
          parse_export( lo_body ).
        WHEN gc_section_start.
          ASSERT 0 = 'todo'.
        WHEN gc_section_element.
          ASSERT 0 = 'todo'.
        WHEN gc_section_code.
          parse_code( lo_body ).
        WHEN gc_section_data.
          ASSERT 0 = 'todo'.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_code.
* todo
    RETURN.
  ENDMETHOD.


  METHOD parse_custom.
* todo
    RETURN.
  ENDMETHOD.


  METHOD parse_export.
* todo
    RETURN.
  ENDMETHOD.


  METHOD parse_function.
* todo
    RETURN.
  ENDMETHOD.


  METHOD parse_type.
* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
