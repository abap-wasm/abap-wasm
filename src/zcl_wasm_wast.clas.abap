class ZCL_WASM_WAST definition
  public
  create public .

public section.

  methods TO_WASM
    returning
      value(RV_WASM) type XSTRING .
  methods CONSTRUCTOR
    importing
      !IV_WAST type STRING .
  methods GET_FIRST_MODULE
    returning
      value(RO_MODULE) type ref to ZCL_WASM_MODULE .
  methods LIST_MODULES .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_WASM_WAST IMPLEMENTATION.


  METHOD CONSTRUCTOR.
* todo
    RETURN.
  ENDMETHOD.


  METHOD GET_FIRST_MODULE.
* todo
    RETURN.
  ENDMETHOD.


  method LIST_MODULES.
  endmethod.


  METHOD TO_WASM.

* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
