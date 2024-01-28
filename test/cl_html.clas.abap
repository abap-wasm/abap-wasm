CLASS cl_html DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS render RETURNING VALUE(rv_html) TYPE string.

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

  PRIVATE SECTION.
    DATA mv_html TYPE string.

    DATA mv_errors TYPE i.
    DATA mv_warnings TYPE i.
    DATA mv_success TYPE i.

    DATA mo_type_descr TYPE REF TO cl_abap_typedescr.
ENDCLASS.

CLASS cl_html IMPLEMENTATION.

  METHOD render.
    mv_html = mv_html && |<br>\n|.
    mv_html = mv_html && |<h2>Errors: { mv_errors }</h2>\n|.
    mv_html = mv_html && |<h2>Warnings: { mv_warnings }</h2>\n|.
    mv_html = mv_html && |<h2>Successes: { mv_success }</h2>\n|.

    rv_html = mv_html.
  ENDMETHOD.

  METHOD add_suite.
    mv_html = mv_html && |<h1>{ iv_suite }</h1>\n|.
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
