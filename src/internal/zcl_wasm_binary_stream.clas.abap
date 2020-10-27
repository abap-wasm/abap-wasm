class ZCL_WASM_BINARY_STREAM definition
  public
  create public .

public section.

  methods CONSTRUCTOR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS peek .
    METHODS shift .
ENDCLASS.



CLASS ZCL_WASM_BINARY_STREAM IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD get_length.
  ENDMETHOD.


  METHOD peek.
  ENDMETHOD.


  METHOD shift.
  ENDMETHOD.
ENDCLASS.
