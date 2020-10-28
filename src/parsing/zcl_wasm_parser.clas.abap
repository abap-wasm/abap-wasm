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



    CONSTANTS lc_magic TYPE x LENGTH 4 VALUE '0061736D'.
    CONSTANTS lc_version TYPE x LENGTH 4 VALUE '01000000'.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_wasm ).

* https://webassembly.github.io/spec/core/binary/modules.html#binary-module
    ASSERT lo_stream->shift( 4 ) = lc_magic.
    ASSERT lo_stream->shift( 4 ) = lc_version.

    WHILE lo_stream->get_length( ) > 0.
* https://webassembly.github.io/spec/core/binary/modules.html#sections
      DATA(lv_section) = lo_stream->shift( 1 ).
      DATA(lv_length) = lo_stream->shift_int( ).
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

* https://webassembly.github.io/spec/core/binary/modules.html#binary-codesec

    DATA(lv_code_count) = io_body->shift_int( ).

    DO lv_code_count TIMES.

      DATA(lv_code_size) = io_body->shift_int( ).

      DATA(lo_code) = NEW zcl_wasm_binary_stream( io_body->shift( lv_code_size ) ).

      DATA(lv_locals_count) = lo_code->shift_int( ).
      ASSERT lv_locals_count = 0. " todo

      DATA(lv_instructions) = lo_code->get_data( ).

    ENDDO.

  ENDMETHOD.


  METHOD parse_custom.
* todo
    RETURN.
  ENDMETHOD.


  METHOD parse_export.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-exportsec

    DATA(lv_export_count) = io_body->shift_int( ).

    DO lv_export_count TIMES.

      DATA(lv_name) = io_body->shift_utf8( ).

      DATA(lv_type) = io_body->shift( 1 ).
      CASE lv_type.
        WHEN '00'.
          DATA(lv_funcidx) = io_body->shift_int( ).
        WHEN '01'.
          DATA(lv_tableidx) = io_body->shift_int( ).
        WHEN '02'.
          DATA(lv_memidx) = io_body->shift_int( ).
        WHEN '03'.
          DATA(lv_globalidx) = io_body->shift_int( ).
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.

    ENDDO.

  ENDMETHOD.


  METHOD parse_function.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec

    DATA(lv_function_count) = io_body->shift_int( ).

    DO lv_function_count TIMES.
      DATA(lv_typeidx) = io_body->shift_int( ).
    ENDDO.

  ENDMETHOD.


  METHOD parse_type.

* https://webassembly.github.io/spec/core/binary/modules.html#type-section

    DATA(lv_type_count) = io_body->shift_int( ).

    DO lv_type_count TIMES.
      DATA(lv_type) = io_body->shift( 1 ).

      ASSERT lv_type = zcl_wasm_types=>c_function_type.

      DATA(lv_parameter_count) = io_body->shift_int( ).
      DATA(lv_parameter_types) = io_body->shift( lv_parameter_count ).

      DATA(lv_result_count) = io_body->shift_int( ).
      DATA(lv_result_types) = io_body->shift( lv_result_count ).

    ENDDO.

  ENDMETHOD.
ENDCLASS.
