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
ENDCLASS.



CLASS ZCL_WAST_PARSER IMPLEMENTATION.


  METHOD parse.

    DATA lv_normalized TYPE string.
    lv_normalized = iv_wast.
    REPLACE ALL OCCURRENCES OF |\n| IN lv_normalized WITH | |.
    CONDENSE lv_normalized.

    BREAK-POINT.

  ENDMETHOD.
ENDCLASS.
