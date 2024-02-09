class ZCX_WASM_BRANCH definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data DEPTH type INT8 .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !DEPTH type INT8 optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_WASM_BRANCH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->DEPTH = DEPTH .
  endmethod.
ENDCLASS.
