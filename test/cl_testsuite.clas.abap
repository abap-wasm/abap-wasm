CLASS cl_testsuite DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
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
        it_files  TYPE ty_files.
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

    run_folder(
      iv_folder = lv_folder
      it_files  = lt_files ).

    WRITE / '@KERNEL }'.

  ENDMETHOD.

  METHOD run_folder.

    DATA ls_json TYPE ty_json.

    READ TABLE it_files WITH KEY filename = |{ iv_folder }.json| INTO DATA(ls_file).
    ASSERT sy-subrc = 0.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = cl_abap_codepage=>convert_from( ls_file-hex )
      CHANGING
        data = ls_json ).

    WRITE / ls_json-source_filename.

  ENDMETHOD.

ENDCLASS.
