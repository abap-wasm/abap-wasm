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
        iv_start_time TYPE i OPTIONAL
        iv_text       TYPE string.

    METHODS add_warning
      IMPORTING
        iv_start_time TYPE i OPTIONAL
        iv_text       TYPE string.

    METHODS add_error
      IMPORTING
        iv_start_time TYPE i OPTIONAL
        iv_text       TYPE string.

    METHODS add_command
      IMPORTING
        is_command TYPE cl_testsuite=>ty_json_commands
        iv_wast    TYPE string.

    METHODS add_suite
      IMPORTING
        iv_suite TYPE string.

    METHODS end.

  PRIVATE SECTION.
    DATA mv_html     TYPE string.
    DATA mv_start    TYPE timestamp.
    DATA mv_end      TYPE timestamp.
    DATA mv_errors   TYPE i.
    DATA mv_warnings TYPE i.
    DATA mv_success  TYPE i.
    DATA mt_suites   TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA mo_type_descr TYPE REF TO cl_abap_typedescr.
ENDCLASS.

CLASS cl_result IMPLEMENTATION.

  METHOD constructor.
    GET TIME STAMP FIELD mv_start.
  ENDMETHOD.

  METHOD end.
    GET TIME STAMP FIELD mv_end.
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

    ls_totals-runtime = cl_abap_tstmp=>subtract(
      tstmp1 = mv_end
      tstmp2 = mv_start ).

    rv_json = /ui2/cl_json=>serialize(
      pretty_name = /ui2/cl_json=>pretty_mode-low_case
      data        = ls_totals ).

  ENDMETHOD.

  METHOD render_html.
    DATA lv_top TYPE string.

    lv_top = lv_top && |<a href="./index.html">Home</a><br>|.
    lv_top = lv_top && |<h3>Errors: { mv_errors }</h3>\n|.
    lv_top = lv_top && |<h3>Warnings: { mv_warnings }</h3>\n|.
    lv_top = lv_top && |<h3>Successes: { mv_success }</h3>\n|.
    lv_top = lv_top && |<hr>\n|.

    GET TIME STAMP FIELD DATA(lv_timestamp).
    mv_html = mv_html && |<p>Generated at { lv_timestamp TIMESTAMP = ISO }</p>\n|.

    rv_html = lv_top && mv_html.
  ENDMETHOD.

  METHOD add_suite.
    mv_html = mv_html && |<h1>{ iv_suite }</h1>\n|.
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
    IF is_command-type = 'assert_return'
        OR is_command-type = 'assert_trap'.
      mv_html = mv_html && |<pre>| && iv_wast && |</pre>\n|.
    ENDIF.
  ENDMETHOD.

  METHOD add_warning.
    DATA lv_end_time TYPE i.
    GET RUN TIME FIELD lv_end_time.
    IF iv_start_time IS SUPPLIED.
      lv_end_time = lv_end_time - iv_start_time.
      IF lv_end_time > 10.
        DATA(lv_runtime) = |, { lv_end_time }ms|.
      ENDIF.
    ENDIF.
    mv_warnings = mv_warnings + 1.
    mv_html = mv_html && '<p style="background-color: yellow">' && iv_text && lv_runtime && |</p>\n|.
  ENDMETHOD.

  METHOD add_success.
    DATA lv_end_time TYPE i.
    GET RUN TIME FIELD lv_end_time.
    IF iv_start_time IS SUPPLIED.
      lv_end_time = lv_end_time - iv_start_time.
      IF lv_end_time > 10.
        DATA(lv_runtime) = |, { lv_end_time }ms|.
      ENDIF.
    ENDIF.
    mv_success = mv_success + 1.
    mv_html = mv_html && '<p style="background-color: green">' && iv_text && lv_runtime && |</p>\n|.
  ENDMETHOD.

  METHOD add_error.
    DATA lv_end_time TYPE i.
    GET RUN TIME FIELD lv_end_time.
    IF iv_start_time IS SUPPLIED.
      lv_end_time = lv_end_time - iv_start_time.
      IF lv_end_time > 10.
        DATA(lv_runtime) = |, { lv_end_time }ms|.
      ENDIF.
    ENDIF.
    mv_errors = mv_errors + 1.
    mv_html = mv_html && '<p style="background-color: red">' && iv_text && lv_runtime && |</p>\n|.
  ENDMETHOD.

ENDCLASS.
