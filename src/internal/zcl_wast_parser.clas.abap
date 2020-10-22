CLASS zcl_wast_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !iv_wast TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS module
      IMPORTING
        !iv_wast TYPE string .
ENDCLASS.



CLASS ZCL_WAST_PARSER IMPLEMENTATION.


  METHOD module.

    DATA(lv_wast) = iv_wast.

    ASSERT lv_wast CP '(module *'.
    lv_wast = lv_wast+7.
    CONDENSE lv_wast.

  ENDMETHOD.


  METHOD parse.

    DATA(lv_norm) = iv_wast.
    REPLACE ALL OCCURRENCES OF |\n| IN lv_norm WITH | |.
    CONDENSE lv_norm.

    IF lv_norm CP '(module *'.
      module( lv_norm ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
