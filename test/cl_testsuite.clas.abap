CLASS cl_testsuite DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING
        VALUE(rv_html) TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_file,
             filename TYPE string,
             hex      TYPE xstring,
           END OF ty_file.
    TYPES ty_files TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_type_value,
             type TYPE string,
             value TYPE string,
           END OF ty_type_value.

    TYPES: BEGIN OF ty_json_commands,
             type     TYPE string,
             line     TYPE i,
             filename TYPE string,
             text     TYPE string,
             BEGIN OF action,
                type  TYPE string,
                field TYPE string,
                args  TYPE STANDARD TABLE OF ty_type_value WITH DEFAULT KEY,
             END OF action,
             expected TYPE STANDARD TABLE OF ty_type_value WITH DEFAULT KEY,
           END OF ty_json_commands.

    TYPES: BEGIN OF ty_json,
             source_filename TYPE string,
             commands        TYPE STANDARD TABLE OF ty_json_commands WITH DEFAULT KEY,
           END OF ty_json.

    CLASS-METHODS run_folder
      IMPORTING
        iv_folder TYPE string
        it_files  TYPE ty_files
      RETURNING
        VALUE(rv_html) TYPE string.
ENDCLASS.


CLASS cl_testsuite IMPLEMENTATION.

  METHOD run.

    DATA lv_folder   TYPE string.
    DATA lv_filename TYPE string.
    DATA lv_hex      TYPE xstring.
    DATA lt_files    TYPE ty_files.


    WRITE / '@KERNEL const fs = await import("fs");'.
    WRITE / '@KERNEL const folders = fs.readdirSync("./testsuite/").filter(a => a.includes(".") === false);'.
    WRITE / '@KERNEL for (const folder of folders) {'.
    CLEAR lt_files.
    WRITE / '@KERNEL   lv_folder.set(folder);'.
    WRITE / '@KERNEL   const filenames = fs.readdirSync("./testsuite/" + folder);'.
    WRITE / '@KERNEL   for (const filename of filenames) {'.
    WRITE / '@KERNEL     lv_filename.set(filename);'.
    WRITE / '@KERNEL     lv_hex.set(fs.readFileSync("./testsuite/" + folder + "/" + filename).toString("hex").toUpperCase());'.
    APPEND VALUE #(
      filename = condense( lv_filename )
      hex      = lv_hex ) TO lt_files.
    WRITE / '@KERNEL   }'.

    rv_html = rv_html && run_folder(
      iv_folder = lv_folder
      it_files  = lt_files ).
    " IF 1 = 1.
    "   RETURN.
    " ENDIF.
    WRITE / '@KERNEL }'.

  ENDMETHOD.

  METHOD run_folder.

    DATA ls_json     TYPE ty_json.
    DATA lv_filename TYPE string.
    DATA lo_wasm     TYPE REF TO zif_wasm.
    DATA lv_hex      TYPE xstring.
    DATA lt_skip     TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    READ TABLE it_files WITH KEY filename = |{ iv_folder }.json| INTO DATA(ls_file).
    ASSERT sy-subrc = 0.

    WRITE / '@KERNEL const fs = await import("fs");'.

    " INSERT 'call.0.wasm' INTO TABLE lt_skip. " f64 parsing
    " INSERT 'call_indirect.0.wasm' INTO TABLE lt_skip. " f64 parsing
    " INSERT 'const.55.wasm' INTO TABLE lt_skip. " f64 parsing
    " INSERT 'const.56.wasm' INTO TABLE lt_skip. " f64 parsing

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = cl_abap_codepage=>convert_from( ls_file-hex )
      CHANGING
        data = ls_json ).

    WRITE / ls_json-source_filename.

    rv_html = |<h1>{ ls_json-source_filename }</h1>\n|.
    LOOP AT ls_json-commands INTO DATA(ls_command).
      DATA(lv_command) = /ui2/cl_json=>serialize(
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
        compress    = abap_true
        data        = ls_command ).
      rv_html = rv_html && |<pre>| && lv_command && |</pre>\n|.

      TRY.
          CASE ls_command-type.
            WHEN 'module'.
              READ TABLE lt_skip TRANSPORTING NO FIELDS WITH KEY table_line = ls_command-filename.
              IF sy-subrc = 0.
                rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
              ELSE.
                lv_filename = './testsuite/' && iv_folder && '/' && ls_command-filename.
                WRITE: / 'load:', ls_command-filename.
                WRITE / '@KERNEL lv_hex.set(fs.readFileSync(lv_filename.get()).toString("hex").toUpperCase());'.
*              WRITE / lv_hex.
                lo_wasm = zcl_wasm=>create_with_wasm( lv_hex ).
                rv_html = rv_html && |<p style="background-color: green">loaded</p>\n|.
              ENDIF.
            WHEN 'assert_return'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN 'assert_trap'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN 'assert_malformed'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN 'assert_invalid'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN 'action'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN 'assert_exhaustion'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN 'assert_uninstantiable'.
              rv_html = rv_html && |<p style="background-color: yellow">todo</p>\n|.
            WHEN OTHERS.
              WRITE / ls_command-type.
              ASSERT 1 = 'todo'.
          ENDCASE.
        CATCH cx_static_check INTO DATA(lx_error).
          rv_html = rv_html && |<p style="background-color: red">exception</p>\n|.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
