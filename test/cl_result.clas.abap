CLASS cl_result DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS render_html
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS render_json
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS add_success
      IMPORTING
        iv_success TYPE string.

    METHODS add_warning
      IMPORTING
        iv_warning TYPE string.

    METHODS add_error
      IMPORTING
        iv_error TYPE string.

    METHODS add_command
      IMPORTING
        is_command TYPE cl_testsuite=>ty_json_commands.

    METHODS add_suite
      IMPORTING
        iv_suite TYPE string.

    METHODS end.

  PRIVATE SECTION.
    DATA mv_html     TYPE string.
    DATA mv_start    TYPE i.
    DATA mv_end      TYPE i.
    DATA mv_errors   TYPE i.
    DATA mv_warnings TYPE i.
    DATA mv_success  TYPE i.
    DATA mt_suites   TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA mo_type_descr TYPE REF TO cl_abap_typedescr.
ENDCLASS.

CLASS cl_result IMPLEMENTATION.

  METHOD constructor.
    GET RUN TIME FIELD mv_start.
  ENDMETHOD.

  METHOD end.
    GET RUN TIME FIELD mv_end.
  ENDMETHOD.

  METHOD render_json.

    DATA: BEGIN OF ls_totals,
            errors    TYPE i,
            warnings  TYPE i,
            successes TYPE i,
            runtime   TYPE i,
          END OF ls_totals.

    ls_totals-errors = mv_errors.
    ls_totals-warnings = mv_warnings.
    ls_totals-successes = mv_success.
    ls_totals-runtime = ( mv_end - mv_start ) / 1000.

    rv_json = /ui2/cl_json=>serialize(
      pretty_name = /ui2/cl_json=>pretty_mode-low_case
      data        = ls_totals ).

  ENDMETHOD.

  METHOD render_html.
    DATA lv_top TYPE string.

    LOOP AT mt_suites INTO DATA(ls_suite).
      lv_top = lv_top && |<a href="#{ ls_suite }">{ ls_suite }</a>&nbsp;\n|.
    ENDLOOP.

    lv_top = lv_top && |<hr>\n|.
    lv_top = lv_top && |<h3>Errors: { mv_errors }</h3>\n|.
    lv_top = lv_top && |<h3>Warnings: { mv_warnings }</h3>\n|.
    lv_top = lv_top && |<h3>Successes: { mv_success }</h3>\n|.
    lv_top = lv_top && |<hr>\n|.

    GET TIME STAMP FIELD DATA(lv_timestamp).
    mv_html = mv_html && |<p>Generated at { lv_timestamp TIMESTAMP = ISO }</p>\n|.

    rv_html = lv_top && mv_html.
  ENDMETHOD.

  METHOD add_suite.
    mv_html = mv_html && |<h1 id="{ iv_suite }">{ iv_suite }</h1>\n|.
    APPEND iv_suite TO mt_suites.
  ENDMETHOD.

  METHOD add_command.
    IF mo_type_descr IS INITIAL.
      mo_type_descr = cl_abap_typedescr=>describe_by_data( is_command ).
    ENDIF.

    DATA(lv_command) = /ui2/cl_json=>serialize(
      pretty_name = /ui2/cl_json=>pretty_mode-low_case
      compress    = abap_true
      type_descr  = mo_type_descr
      data        = is_command ).

    mv_html = mv_html && |<pre>| && lv_command && |</pre>\n|.
  ENDMETHOD.

  METHOD add_warning.
    mv_warnings = mv_warnings + 1.
    mv_html = mv_html && '<p style="background-color: yellow">' && iv_warning && |</p>\n|.
  ENDMETHOD.

  METHOD add_success.
    mv_success = mv_success + 1.
    mv_html = mv_html && '<p style="background-color: green">' && iv_success && |</p>\n|.
  ENDMETHOD.

  METHOD add_error.
    mv_errors = mv_errors + 1.
    mv_html = mv_html && '<p style="background-color: red">' && iv_error && |</p>\n|.
  ENDMETHOD.

ENDCLASS.
