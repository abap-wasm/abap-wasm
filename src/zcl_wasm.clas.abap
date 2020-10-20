class ZCL_WASM definition
  public
  create protected .

public section.

  methods EXECUTE_EXPORT .
  methods LIST_EXPORTS .
  methods LIST_IMPORTS .
  class-methods CREATE_WITH_BINARY
    importing
      !IV_WASM type XSTRING .
  class-methods CREATE_WITH_TEXT
    importing
      !IV_WAST type STRING .
  methods CONSTRUCTOR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM IMPLEMENTATION.


  METHOD constructor.
* todo
    RETURN.
  ENDMETHOD.


  METHOD create_with_binary.
* todo
    RETURN.
  ENDMETHOD.


  METHOD create_with_text.
* todo, call new class to convert text to binary
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
