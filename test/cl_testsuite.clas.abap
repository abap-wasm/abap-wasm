CLASS cl_testsuite DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_folder      TYPE string
      RETURNING
        VALUE(ro_html) TYPE REF TO cl_result.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_file,
             filename TYPE string,
             hex      TYPE xstring,
           END OF ty_file.
    TYPES ty_files TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_type_value,
             type  TYPE string,
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
             as       TYPE string,
           END OF ty_json_commands.

    TYPES: BEGIN OF ty_json,
             source_filename TYPE string,
             commands        TYPE STANDARD TABLE OF ty_json_commands WITH DEFAULT KEY,
           END OF ty_json.

    CLASS-DATA go_result TYPE REF TO cl_result.

    CLASS-METHODS run_folder
      IMPORTING
        iv_folder TYPE string
        iv_wast   TYPE string
        it_files  TYPE ty_files.

    CLASS-METHODS assert_return
      IMPORTING
        is_command TYPE ty_json_commands
        io_wasm    TYPE REF TO zif_wasm_module
      RAISING
        zcx_wasm.

    TYPES: BEGIN OF ty_results,
             start_time TYPE i,
             values     TYPE zif_wasm_value=>ty_values,
           END OF ty_results.

    CLASS-METHODS invoke
      IMPORTING
        is_command        TYPE ty_json_commands
        io_wasm           TYPE REF TO zif_wasm_module
      RETURNING
        VALUE(rs_results) TYPE ty_results
      RAISING
        zcx_wasm.
ENDCLASS.


CLASS cl_testsuite IMPLEMENTATION.

  METHOD run.

    DATA lv_filename TYPE string.
    DATA lv_hex      TYPE xstring.
    DATA lv_wast     TYPE xstring.
    DATA lt_files    TYPE ty_files.

    go_result = NEW #( ).

    CLEAR lt_files.
    WRITE '@KERNEL const fs = await import("fs");'.
    WRITE '@KERNEL lv_wast.set(fs.readFileSync("./testsuite/" + iv_folder.get() + ".wast").toString("hex").toUpperCase());'.
    WRITE '@KERNEL const filenames = fs.readdirSync("./testsuite/" + iv_folder.get());'.
    WRITE '@KERNEL for (const filename of filenames) {'.
    WRITE '@KERNEL   lv_filename.set(filename);'.
    WRITE '@KERNEL   lv_hex.set(fs.readFileSync("./testsuite/" + iv_folder.get() + "/" + filename).toString("hex").toUpperCase());'.
    APPEND VALUE #(
      filename = condense( lv_filename )
      hex      = lv_hex ) TO lt_files.
    WRITE '@KERNEL }'.

    run_folder(
      iv_folder = iv_folder
      iv_wast   = cl_abap_codepage=>convert_from( lv_wast )
      it_files  = lt_files ).

    go_result->end( ).
    ro_html = go_result.

  ENDMETHOD.

  METHOD invoke.

    DATA lt_values TYPE zif_wasm_value=>ty_values.

    IF io_wasm IS INITIAL.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |assert_return: nothing loaded|.
    ENDIF.

    GET RUN TIME FIELD rs_results-start_time.

    IF is_command-action-type <> 'invoke'.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |invoke: unknown action type, { is_command-action-type }|.
    ENDIF.

    LOOP AT is_command-action-args INTO DATA(ls_arg).
      CASE ls_arg-type.
        WHEN 'i32'.
          APPEND zcl_wasm_i32=>from_unsigned( CONV #( ls_arg-value ) ) TO lt_values.
        WHEN 'i64'.
          APPEND zcl_wasm_i64=>from_unsigned( ls_arg-value ) TO lt_values.
        WHEN 'f32'.
          APPEND zcl_wasm_f32=>from_unsigned_i32( CONV #( ls_arg-value ) ) TO lt_values.
        WHEN 'f64'.
          APPEND zcl_wasm_f64=>from_unsigned( ls_arg-value ) TO lt_values.
        WHEN 'funcref'.
          IF ls_arg-value = 'null'.
            APPEND NEW zcl_wasm_funcref( -1 ) TO lt_values.
          ELSE.
            APPEND NEW zcl_wasm_funcref( CONV #( ls_arg-value ) ) TO lt_values.
          ENDIF.
        WHEN 'externref'.
          IF ls_arg-value = 'null'.
            APPEND NEW zcl_wasm_externref( -1 ) TO lt_values.
          ELSE.
            APPEND NEW zcl_wasm_externref( CONV #( ls_arg-value ) ) TO lt_values.
          ENDIF.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |unknown type, invoke, { ls_arg-type }|.
      ENDCASE.
    ENDLOOP.

    rs_results-values = io_wasm->execute_function_export(
        iv_name       = is_command-action-field
        it_parameters = lt_values ).

  ENDMETHOD.

  METHOD assert_return.

    DATA(ls_results) = invoke(
      is_command = is_command
      io_wasm    = io_wasm ).

    IF lines( ls_results-values ) <> lines( is_command-expected ).
      go_result->add_error(
        iv_start_time = ls_results-start_time
        iv_text       = |error, wrong number of results| ).
      RETURN.
    ENDIF.

    DATA(lv_error) = abap_false.
    DO lines( ls_results-values ) TIMES.
      DATA(lv_index) = sy-index.
      READ TABLE is_command-expected INDEX lv_index INTO DATA(ls_expected).
      ASSERT sy-subrc = 0.
      READ TABLE ls_results-values INDEX lv_index INTO DATA(li_result).
      ASSERT sy-subrc = 0.

      IF li_result IS INITIAL.
        lv_error = abap_true.
        go_result->add_error(
          iv_start_time = ls_results-start_time
          iv_text       = |error, expected non initial value| ).
        EXIT. " current loop
      ENDIF.

      TRY.
          CASE ls_expected-type.
            WHEN 'i32'.
              DATA(lv_expected) = CONV int8( ls_expected-value ).
              DATA(lv_result)   = CAST zcl_wasm_i32( li_result )->get_unsigned( ).
              IF lv_expected <> lv_result.
                lv_error = abap_true.
                go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |error, wrong result, expected { lv_expected }, got { lv_result }| ).
                EXIT. " current loop
              ENDIF.
            WHEN 'i64'.
              DATA(lv_str) = CAST zcl_wasm_i64( li_result )->get_unsigned( ).
              IF ls_expected-value <> lv_str.
                lv_error = abap_true.
                go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |error, wrong result, expected { ls_expected-value }, got { lv_str }| ).
                EXIT. " current loop
              ENDIF.
            WHEN 'f32'.
              lv_expected = CONV int8( ls_expected-value ).
              lv_result   = CAST zcl_wasm_f32( li_result )->get_unsigned_i32( ).
              IF lv_expected <> lv_result.
                lv_error = abap_true.
                go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |error, wrong result, expected { lv_expected }, got { lv_result }| ).
                EXIT. " current loop
              ENDIF.
            WHEN 'f64'.
              lv_str = CAST zcl_wasm_f64( li_result )->get_unsigned( ).
              IF ls_expected-value <> lv_str.
                lv_error = abap_true.
                go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |error, wrong result, expected { ls_expected-value }, got { lv_str }| ).
                EXIT. " current loop
              ENDIF.
            WHEN 'externref'.
              DATA(lo_externref) = CAST zcl_wasm_externref( li_result ).
              IF ls_expected-value = 'null'.
                IF lo_externref->is_null( ) = abap_false.
                  lv_error = abap_true.
                  go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |externref not null| ).
                  EXIT. " current loop
                ENDIF.
              ELSE.
                lv_expected = CONV int8( ls_expected-value ).
                IF lv_expected <> lo_externref->get_address( ).
                  lv_error = abap_true.
                  go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |externref unexpected value, { li_result->human_readable_value( ) }| ).
                  EXIT. " current loop
                ENDIF.
              ENDIF.
            WHEN 'funcref'.
              DATA(lo_funcref) = CAST zcl_wasm_funcref( li_result ).
              IF ls_expected-value = 'null'.
                IF lo_funcref->is_null( ) = abap_false.
                  lv_error = abap_true.
                  go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |funcref not null| ).
                  EXIT. " current loop
                ENDIF.
              ELSE.
                lv_expected = CONV int8( ls_expected-value ).
                IF lv_expected <> lo_funcref->get_address( ).
                  lv_error = abap_true.
                  go_result->add_error(
                    iv_start_time = ls_results-start_time
                    iv_text       = |funcref unexpected value, { li_result->human_readable_value( ) }| ).
                  EXIT. " current loop
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              lv_error = abap_true.
              go_result->add_error(
                iv_start_time = ls_results-start_time
                iv_text       = |unknown type, assert_return: { ls_expected-type }| ).
              EXIT. " current loop
          ENDCASE.
        CATCH cx_sy_move_cast_error.
          lv_error = abap_true.
          go_result->add_error(
            iv_start_time = ls_results-start_time
            iv_text       = |assert_return, wrong type, casting failed, got { li_result->get_type( ) }| ).
          EXIT. " current loop
      ENDTRY.
    ENDDO.

    IF lv_error = abap_false.
      go_result->add_success(
          iv_start_time = ls_results-start_time
          iv_text       = |ok, result| ).
    ENDIF.

  ENDMETHOD.

  METHOD run_folder.

    DATA ls_json     TYPE ty_json.
    DATA lv_filename TYPE string.
    DATA lo_wasm     TYPE REF TO zif_wasm_module.
    DATA lv_wast     TYPE string.
    DATA lv_hex      TYPE xstring.
    DATA lt_imports  TYPE zif_wasm_types=>ty_imports_tt.


    READ TABLE it_files WITH KEY filename = |{ iv_folder }.json| ASSIGNING FIELD-SYMBOL(<ls_file>).
    IF sy-subrc <> 0.
      WRITE / iv_folder.
      ASSERT sy-subrc = 0.
    ENDIF.

    WRITE / '@KERNEL const fs = await import("fs");'.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = cl_abap_codepage=>convert_from( <ls_file>-hex )
      CHANGING
        data = ls_json ).

    WRITE / '================================'.
    WRITE / ls_json-source_filename.

    SPLIT iv_wast AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_wast).

    lt_imports = VALUE #( (
      name   = 'spectest'
      module = NEW cl_spectest( ) ) ).

    go_result->add_suite( ls_json-source_filename ).

    LOOP AT ls_json-commands ASSIGNING FIELD-SYMBOL(<ls_command>).
      CLEAR lv_wast.
      READ TABLE lt_wast INDEX <ls_command>-line INTO lv_wast.
      go_result->add_command(
        is_command = <ls_command>
        iv_wast    = lv_wast ).

      lv_filename = './testsuite/' && iv_folder && '/' && <ls_command>-filename.
      TRY.
          CASE <ls_command>-type.
            WHEN 'module'.
              WRITE / '@KERNEL lv_hex.set(fs.readFileSync(lv_filename.get()).toString("hex").toUpperCase());'.
              lo_wasm = zcl_wasm=>create_with_wasm(
                it_imports = lt_imports
                iv_wasm    = lv_hex ).
              go_result->add_success( |loaded| ).
            WHEN 'assert_return'.
              assert_return(
                is_command = <ls_command>
                io_wasm    = lo_wasm ).
            WHEN 'assert_trap'.
              TRY.
                  invoke(
                    is_command = <ls_command>
                    io_wasm    = lo_wasm ).
                  go_result->add_error( |error, expected trap| ).
                CATCH cx_root INTO DATA(lx_error).
                  IF lx_error->get_text( ) CP '*todo*'.
                    go_result->add_error( |assert_trap, todo: { lx_error->get_text( ) }| ).
                  ELSE.
                    go_result->add_success( |ok, got error: { lx_error->get_text( ) }| ).
                  ENDIF.
              ENDTRY.
            WHEN 'assert_malformed'
                OR 'assert_invalid'
                OR 'assert_uninstantiable'.
              WRITE / '@KERNEL lv_hex.set(fs.readFileSync(lv_filename.get()).toString("hex").toUpperCase());'.
              TRY.
                  DATA(li_wasm) = zcl_wasm=>create_with_wasm( lv_hex ).
                  IF <ls_command>-type = 'assert_uninstantiable'.
                    li_wasm->instantiate( ).
                  ENDIF.
                  go_result->add_error( |expected error| ).
                CATCH cx_root INTO lx_error.
                  go_result->add_success( |got error: { lx_error->get_text( ) }| ).
              ENDTRY.
            WHEN 'action'.
              invoke(
                is_command = <ls_command>
                io_wasm    = lo_wasm ).
              go_result->add_success( |action done| ).
            WHEN 'assert_exhaustion'.
              go_result->add_warning( |todo, assert_exhaustion| ).
            WHEN 'register'.
              lo_wasm->instantiate( ).
              INSERT VALUE #(
                name   = <ls_command>-as
                module = lo_wasm ) INTO TABLE lt_imports.
              go_result->add_success( |registered| ).
            WHEN 'assert_unlinkable'.
              go_result->add_warning( |todo, assert_unlinkable| ).
            WHEN OTHERS.
              ASSERT 1 = 'todo'.
          ENDCASE.
        CATCH cx_root INTO lx_error.
          DATA(lv_text) = lx_error->get_text( ).
          go_result->add_error( |exception: { lv_text }| ).
          " IF lv_text = |An exception was raised.|.
          "   WRITE / '@KERNEL console.dir(lx_error);'.
          " ENDIF.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
