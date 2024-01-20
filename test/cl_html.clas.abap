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
ENDCLASS.

CLASS cl_html IMPLEMENTATION.

  METHOD render.
    rv_html = mv_html.
  ENDMETHOD.

  METHOD add_suite.
    mv_html = mv_html && |<h1>{ iv_suite }</h1>\n|.
  ENDMETHOD.

  METHOD add_command.
    DATA(lv_command) = /ui2/cl_json=>serialize(
      pretty_name = /ui2/cl_json=>pretty_mode-low_case
      compress    = abap_true
      data        = is_command ).

    mv_html = mv_html && |<pre>| && lv_command && |</pre>\n|.
  ENDMETHOD.

  METHOD add_warning.
    mv_html = mv_html && '<p style="background-color: yellow">' && iv_warning && |</p>\n|.
  ENDMETHOD.

  METHOD add_success.
    mv_html = mv_html && '<p style="background-color: green">' && iv_success && |</p>\n|.
  ENDMETHOD.

  METHOD add_error.
    mv_html = mv_html && '<p style="background-color: red">' && iv_error && |</p>\n|.
  ENDMETHOD.

ENDCLASS.
