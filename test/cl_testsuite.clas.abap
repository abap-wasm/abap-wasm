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

    CLASS-METHODS assert_return
      IMPORTING
        is_command TYPE ty_json_commands
        io_wasm    TYPE REF TO zif_wasm
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

    WRITE / '@KERNEL }'.

  ENDMETHOD.

  METHOD assert_return.

    DATA lt_values   TYPE zif_wasm_value=>ty_values.

    IF is_command-action-type = 'invoke'.
      LOOP AT is_command-action-args INTO DATA(ls_arg).
        CASE ls_arg-type.
          WHEN 'i32'.
            APPEND NEW zcl_wasm_i32( CONV #( ls_arg-value ) ) TO lt_values.
          " WHEN 'f32'.
          "   APPEND NEW zcl_wasm_f32( CONV #( ls_arg-value ) ) TO lt_values.
          WHEN OTHERS.
            rv_html = rv_html && |<p style="background-color: yellow">unknown type, { ls_arg-type }</p>\n|.
        ENDCASE.
      ENDLOOP.

      IF is_command-action-field = 'call'
          OR is_command-action-field = 'call Mf.call'
          OR is_command-action-field = 'Mf.call'.
        RAISE EXCEPTION NEW zcx_wasm( text = 'call todo' ).
      ENDIF.

      WRITE / |excecute { is_command-action-field }|.
      DATA(lt_result) = io_wasm->execute_function_export(
        iv_name       = is_command-action-field
        it_parameters = lt_values ).

      IF lines( lt_result ) <> lines( is_command-expected ).
        rv_html = rv_html && |<p style="background-color: yellow">error, wrong number of results</p>\n|.
        RETURN.
      ENDIF.

      DATA(lv_error) = abap_false.
      DO lines( lt_result ) TIMES.
        DATA(lv_index) = sy-index.
        READ TABLE is_command-expected INDEX lv_index INTO DATA(ls_expected).
        ASSERT sy-subrc = 0.
        READ TABLE lt_result INDEX lv_index INTO DATA(ls_result).
        ASSERT sy-subrc = 0.

        CASE ls_expected-type.
          WHEN 'i32'.
            DATA(lv_expected) = CONV i( ls_expected-value ).
            DATA(lv_result)   = CAST zcl_wasm_i32( ls_result )->get_value( ).
            IF lv_expected <> lv_result.
              lv_error = abap_true.
              rv_html = rv_html && |<p style="background-color: yellow">error, wrong result, expected { lv_expected }, got { lv_result }</p>\n|.
            ELSE.
              rv_html = rv_html && |<p style="background-color: green">ok</p>\n|.
            ENDIF.
          " WHEN 'f32'.
          "   APPEND NEW zcl_wasm_f32( CONV #( ls_arg-value ) ) TO lt_values.
          WHEN OTHERS.
            rv_html = rv_html && |<p style="background-color: yellow">unknown type, assert_return: { ls_arg-type }</p>\n|.
        ENDCASE.
      ENDDO.

      IF lv_error = abap_false.
        rv_html = rv_html && |<p style="background-color: green">ok, result</p>\n|.
      ELSE.
        rv_html = rv_html && |<p style="background-color: red">error, result</p>\n|.
      ENDIF.
    ELSE.
      rv_html = rv_html && |<p style="background-color: yellow">todo, { is_command-action-type }</p>\n|.
    ENDIF.

  ENDMETHOD.

  METHOD run_folder.

    DATA ls_json     TYPE ty_json.
    DATA lv_filename TYPE string.
    DATA lo_wasm     TYPE REF TO zif_wasm.
    DATA lv_hex      TYPE xstring.


    READ TABLE it_files WITH KEY filename = |{ iv_folder }.json| INTO DATA(ls_file).
    ASSERT sy-subrc = 0.

    WRITE / '@KERNEL const fs = await import("fs");'.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = cl_abap_codepage=>convert_from( ls_file-hex )
      CHANGING
        data = ls_json ).

    WRITE / '================================'.
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
              lv_filename = './testsuite/' && iv_folder && '/' && ls_command-filename.
              WRITE / |load: { ls_command-filename }|.
              WRITE / '@KERNEL lv_hex.set(fs.readFileSync(lv_filename.get()).toString("hex").toUpperCase());'.
              lo_wasm = zcl_wasm=>create_with_wasm( lv_hex ).
              rv_html = rv_html && |<p style="background-color: green">loaded</p>\n|.
            WHEN 'assert_return'.
              rv_html = rv_html && assert_return(
                is_command = ls_command
                io_wasm    = lo_wasm ).
            WHEN 'assert_trap'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, assert_trap</p>\n|.
            WHEN 'assert_malformed'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, assert_malformed</p>\n|.
            WHEN 'assert_invalid'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, assert_invalid</p>\n|.
            WHEN 'action'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, action</p>\n|.
            WHEN 'assert_exhaustion'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, assert_exhaustion</p>\n|.
            WHEN 'assert_uninstantiable'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, assert_uninstantiable</p>\n|.
            WHEN 'register'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, register</p>\n|.
            WHEN 'assert_unlinkable'.
              rv_html = rv_html && |<p style="background-color: yellow">todo, assert_unlinkable</p>\n|.
            WHEN OTHERS.
              WRITE / ls_command-type.
              ASSERT 1 = 'todo'.
          ENDCASE.
        CATCH cx_static_check INTO DATA(lx_error).
          rv_html = rv_html && |<p style="background-color: red">exception: { lx_error->get_text( ) }</p>\n|.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
