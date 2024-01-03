CLASS cl_testsuite DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_file,
             filename TYPE string,
             hex      TYPE xstring,
           END OF ty_file.
    TYPES ty_files TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

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
    APPEND VALUE #( filename = lv_filename hex = lv_hex ) TO lt_files.
    WRITE / '@KERNEL   }'.

    run_folder(
      iv_folder = lv_folder
      it_files  = lt_files ).

    WRITE / '@KERNEL }'.

  ENDMETHOD.

  METHOD run_folder.

    WRITE / iv_folder.

    WRITE: / 'files:', lines( it_files ).

  ENDMETHOD.

ENDCLASS.
