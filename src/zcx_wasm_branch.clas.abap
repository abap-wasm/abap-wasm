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

  methods IF_MESSAGE~GET_TEXT
    redefinition .
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


  METHOD if_message~get_text.

    result = 'Internal ABAP-WASM branching exception'.

  ENDMETHOD.
ENDCLASS.
