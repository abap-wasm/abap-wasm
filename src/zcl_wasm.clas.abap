class ZCL_WASM definition
  public
  create protected .

public section.

  class-methods CREATE_WITH_WASM
    importing
      !IV_WASM type XSTRING
    returning
      value(RO_WASM) type ref to ZCL_WASM .
  class-methods CREATE_WITH_WAT
    importing
      !IV_WAST type STRING
    returning
      value(RO_WASM) type ref to ZCL_WASM .
  class-methods CREATE_WITH_WAST
    importing
      !IV_WAST type STRING
    returning
      value(RO_WASM) type ref to ZCL_WASM .
  methods CONSTRUCTOR
    importing
      !IV_WASM type XSTRING .
  methods EXECUTE_EXPORT .
  methods LIST_EXPORTS .
  methods LIST_IMPORTS .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM IMPLEMENTATION.


  METHOD constructor.
* todo
    RETURN.
  ENDMETHOD.


  METHOD create_with_wasm.

    ro_wasm = NEW zcl_wasm( iv_wasm ).

  ENDMETHOD.


  METHOD create_with_wast.

* todo
    RETURN.

  ENDMETHOD.


  METHOD create_with_wat.

* todo
    RETURN.

  ENDMETHOD.


  METHOD execute_export.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_exports.
* todo
    RETURN.
  ENDMETHOD.


  METHOD list_imports.
* todo
    RETURN.
  ENDMETHOD.
ENDCLASS.
