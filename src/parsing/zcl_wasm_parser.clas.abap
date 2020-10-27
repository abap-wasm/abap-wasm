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
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_PARSER IMPLEMENTATION.


  METHOD parse.

    CONSTANTS lc_magic TYPE x LENGTH 4 VALUE '0061736D'.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_wasm ).

* https://webassembly.github.io/spec/core/binary/modules.html#binary-module
    ASSERT lo_stream->shift( 4 ) = lc_magic. " magic number
    ASSERT lo_stream->shift( 4 ) = '01000000'. " version

* todo

  ENDMETHOD.
ENDCLASS.