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

    CLASS-DATA go_html TYPE REF TO cl_html.

    CLASS-METHODS run_folder
      IMPORTING
        iv_folder TYPE string
        it_files  TYPE ty_files.

    CLASS-METHODS assert_return
      IMPORTING
        is_command TYPE ty_json_commands
        io_wasm    TYPE REF TO zif_wasm.
ENDCLASS.


CLASS cl_testsuite IMPLEMENTATION.

  METHOD run.

    DATA lv_folder   TYPE string.
    DATA lv_filename TYPE string.
    DATA lv_hex      TYPE xstring.
    DATA lt_files    TYPE ty_files.

    go_html = NEW #( ).

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

    run_folder(
      iv_folder = lv_folder
      it_files  = lt_files ).

    WRITE / '@KERNEL }'.

    rv_html = go_html->render( ).

  ENDMETHOD.

  METHOD assert_return.

    DATA lt_values TYPE zif_wasm_value=>ty_values.

    IF is_command-action-type = 'invoke'.
      LOOP AT is_command-action-args INTO DATA(ls_arg).
        CASE ls_arg-type.
          WHEN 'i32'.
            APPEND NEW zcl_wasm_i32( CONV #( ls_arg-value ) ) TO lt_values.
          " WHEN 'f32'.
          "   APPEND NEW zcl_wasm_f32( CONV #( ls_arg-value ) ) TO lt_values.
          WHEN OTHERS.
            go_html->add_warning( |unknown type, { ls_arg-type }| ).
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
        go_html->add_warning( |error, wrong number of results| ).
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
              go_html->add_warning( |error, wrong result, expected { lv_expected }, got { lv_result }| ).
            ELSE.
              go_html->add_success( |ok| ).
            ENDIF.
          " WHEN 'f32'.
          "   APPEND NEW zcl_wasm_f32( CONV #( ls_arg-value ) ) TO lt_values.
          WHEN OTHERS.
            go_html->add_warning( |unknown type, assert_return: { ls_arg-type }| ).
        ENDCASE.
      ENDDO.

      IF lv_error = abap_false.
        go_html->add_success( |ok, result| ).
      ELSE.
        go_html->add_error( |error, result| ).
      ENDIF.
    ELSE.
      go_html->add_warning( |todo, { is_command-action-type }| ).
    ENDIF.

  ENDMETHOD.

  METHOD run_folder.

    DATA ls_json     TYPE ty_json.
    DATA lv_filename TYPE string.
    DATA lo_wasm     TYPE REF TO zif_wasm.
    DATA lv_hex      TYPE xstring.


    READ TABLE it_files WITH KEY filename = |{ iv_folder }.json| ASSIGNING FIELD-SYMBOL(<ls_file>).
    ASSERT sy-subrc = 0.

    WRITE / '@KERNEL const fs = await import("fs");'.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = cl_abap_codepage=>convert_from( <ls_file>-hex )
      CHANGING
        data = ls_json ).

    WRITE / '================================'.
    WRITE / ls_json-source_filename.

    go_html->add_suite( ls_json-source_filename ).

    LOOP AT ls_json-commands ASSIGNING FIELD-SYMBOL(<ls_command>).
      go_html->add_command( <ls_command> ).

      TRY.
          CASE <ls_command>-type.
            WHEN 'module'.
              lv_filename = './testsuite/' && iv_folder && '/' && <ls_command>-filename.
              WRITE / |load: { <ls_command>-filename }|.
              WRITE / '@KERNEL lv_hex.set(fs.readFileSync(lv_filename.get()).toString("hex").toUpperCase());'.
              lo_wasm = zcl_wasm=>create_with_wasm( lv_hex ).
              go_html->add_success( |loaded| ).
            WHEN 'assert_return'.
              assert_return(
                is_command = <ls_command>
                io_wasm    = lo_wasm ).
            WHEN 'assert_trap'.
              go_html->add_warning( |todo, assert_trap| ).
            WHEN 'assert_malformed'.
              go_html->add_warning( |todo, assert_malformed| ).
            WHEN 'assert_invalid'.
              go_html->add_warning( |todo, assert_invalid| ).
            WHEN 'action'.
              go_html->add_warning( |todo, action| ).
            WHEN 'assert_exhaustion'.
              go_html->add_warning( |todo, assert_exhaustion| ).
            WHEN 'assert_uninstantiable'.
              go_html->add_warning( |todo, assert_uninstantiable| ).
            WHEN 'register'.
              go_html->add_warning( |todo, register| ).
            WHEN 'assert_unlinkable'.
              go_html->add_warning( |todo, assert_unlinkable| ).
            WHEN OTHERS.
              WRITE / <ls_command>-type.
              ASSERT 1 = 'todo'.
          ENDCASE.
        CATCH cx_root INTO DATA(lx_error).
          go_html->add_error( |exception: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
