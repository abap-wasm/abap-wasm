class ZCL_WASM_PARSER definition
  public
  create public .

public section.
protected section.
private section.

  methods PARSE
    importing
      !IV_WASM type XSTRING
    returning
      value(RO_MODULE) type ref to ZCL_WASM_MODULE .
ENDCLASS.



CLASS ZCL_WASM_PARSER IMPLEMENTATION.


  METHOD parse.

* todo
    RETURN.

  ENDMETHOD.
ENDCLASS.
