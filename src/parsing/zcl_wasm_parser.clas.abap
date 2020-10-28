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
    CONSTANTS: BEGIN OF gc_section,
                 custom   TYPE x LENGTH 1 VALUE '00',
                 type     TYPE x LENGTH 1 VALUE '01',
                 import   TYPE x LENGTH 1 VALUE '02',
                 function TYPE x LENGTH 1 VALUE '03',
                 table    TYPE x LENGTH 1 VALUE '04',
                 memory   TYPE x LENGTH 1 VALUE '05',
                 global   TYPE x LENGTH 1 VALUE '06',
                 export   TYPE x LENGTH 1 VALUE '07',
                 start    TYPE x LENGTH 1 VALUE '08',
                 element  TYPE x LENGTH 1 VALUE '09',
                 code     TYPE x LENGTH 1 VALUE '10',
                 data     TYPE x LENGTH 1 VALUE '11',
               END OF gc_section.

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

* https://webassembly.github.io/spec/core/binary/modules.html#sections
    DATA(lv_section) = lo_stream->shift( 1 ).
    DATA(lv_length) = lo_stream->shift( 1 ).
* todo

  ENDMETHOD.
ENDCLASS.
