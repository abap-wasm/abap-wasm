CLASS zcl_wasm_binary_stream DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_data TYPE xstring .
    METHODS get_length
      RETURNING
        VALUE(rv_length) TYPE i .
    METHODS get_data
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS peek
      IMPORTING
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS shift
      IMPORTING
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS shift_int
      RETURNING
        VALUE(rv_int) TYPE i .
    METHODS shift_u32
      RETURNING
        VALUE(rv_int) TYPE i .
    METHODS shift_utf8
      RETURNING
        VALUE(rv_name) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring .
ENDCLASS.



CLASS zcl_wasm_binary_stream IMPLEMENTATION.


  METHOD constructor.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD get_data.

    rv_data = mv_data.

  ENDMETHOD.


  METHOD get_length.

    rv_length = xstrlen( mv_data ).

  ENDMETHOD.


  METHOD peek.

    rv_data = mv_data(iv_length).

  ENDMETHOD.


  METHOD shift.

    rv_data = peek( iv_length ).

    mv_data = mv_data+iv_length.

  ENDMETHOD.


  METHOD shift_int.

* todo, this should be LEB128
* todo, deprecate this method, use the typed methods instead
* https://webassembly.github.io/spec/core/binary/values.html#binary-int

    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = shift( 1 ).
    rv_int = CONV i( lv_hex ).

  ENDMETHOD.

  METHOD shift_u32.

* https://webassembly.github.io/spec/core/binary/values.html#binary-int
* https://en.wikipedia.org/wiki/LEB128

    DATA lv_hex TYPE x LENGTH 1.
    DATA lv_bit TYPE c LENGTH 1.
    DATA lv_shift TYPE i VALUE 1.

    DO.
      lv_hex = shift( 1 ).

      GET BIT 1 OF lv_hex INTO lv_bit.
      SET BIT 1 OF lv_hex TO 0.

      rv_int = rv_int + CONV i( lv_hex ) * lv_shift.

      IF lv_bit = '0'.
        RETURN.
      ENDIF.

      lv_shift = lv_shift * 128.

    ENDDO.

  ENDMETHOD.


  METHOD shift_utf8.

* https://webassembly.github.io/spec/core/binary/values.html#names

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( shift( shift_int( ) ) ).

    WRITE / lo_stream->get_data( ).

    WHILE lo_stream->get_length( ) > 0.
      CASE lo_stream->shift( 1 ).
        WHEN '41'.
          CONCATENATE rv_name 'A' INTO rv_name.
        WHEN '42'.
          CONCATENATE rv_name 'B' INTO rv_name.
        WHEN '43'.
          CONCATENATE rv_name 'C' INTO rv_name.
        WHEN '44'.
          CONCATENATE rv_name 'D' INTO rv_name.
        WHEN '45'.
          CONCATENATE rv_name 'E' INTO rv_name.
        WHEN '46'.
          CONCATENATE rv_name 'F' INTO rv_name.
        WHEN '47'.
          CONCATENATE rv_name 'G' INTO rv_name.
        WHEN '48'.
          CONCATENATE rv_name 'H' INTO rv_name.
        WHEN '49'.
          CONCATENATE rv_name 'I' INTO rv_name.
        WHEN '4A'.
          CONCATENATE rv_name 'J' INTO rv_name.
        WHEN '4B'.
          CONCATENATE rv_name 'K' INTO rv_name.
        WHEN '4C'.
          CONCATENATE rv_name 'L' INTO rv_name.
        WHEN '4D'.
          CONCATENATE rv_name 'M' INTO rv_name.
        WHEN '4E'.
          CONCATENATE rv_name 'N' INTO rv_name.
        WHEN '4F'.
          CONCATENATE rv_name 'O' INTO rv_name.
        WHEN '50'.
          CONCATENATE rv_name 'P' INTO rv_name.
        WHEN '51'.
          CONCATENATE rv_name 'Q' INTO rv_name.
        WHEN '52'.
          CONCATENATE rv_name 'R' INTO rv_name.
        WHEN '53'.
          CONCATENATE rv_name 'S' INTO rv_name.
        WHEN '54'.
          CONCATENATE rv_name 'T' INTO rv_name.
        WHEN '55'.
          CONCATENATE rv_name 'U' INTO rv_name.
        WHEN '56'.
          CONCATENATE rv_name 'V' INTO rv_name.
        WHEN '57'.
          CONCATENATE rv_name 'W' INTO rv_name.
        WHEN '58'.
          CONCATENATE rv_name 'X' INTO rv_name.
        WHEN '59'.
          CONCATENATE rv_name 'Y' INTO rv_name.
        WHEN '5A'.
          CONCATENATE rv_name 'Z' INTO rv_name.
        WHEN '61'.
          CONCATENATE rv_name 'a' INTO rv_name.
        WHEN '62'.
          CONCATENATE rv_name 'b' INTO rv_name.
        WHEN '63'.
          CONCATENATE rv_name 'c' INTO rv_name.
        WHEN '64'.
          CONCATENATE rv_name 'd' INTO rv_name.
        WHEN '65'.
          CONCATENATE rv_name 'e' INTO rv_name.
        WHEN '66'.
          CONCATENATE rv_name 'f' INTO rv_name.
        WHEN '67'.
          CONCATENATE rv_name 'g' INTO rv_name.
        WHEN '68'.
          CONCATENATE rv_name 'h' INTO rv_name.
        WHEN '69'.
          CONCATENATE rv_name 'i' INTO rv_name.
        WHEN '6A'.
          CONCATENATE rv_name 'j' INTO rv_name.
        WHEN '6B'.
          CONCATENATE rv_name 'k' INTO rv_name.
        WHEN '6C'.
          CONCATENATE rv_name 'l' INTO rv_name.
        WHEN '6D'.
          CONCATENATE rv_name 'm' INTO rv_name.
        WHEN '6E'.
          CONCATENATE rv_name 'n' INTO rv_name.
        WHEN '6F'.
          CONCATENATE rv_name 'o' INTO rv_name.
        WHEN '70'.
          CONCATENATE rv_name 'p' INTO rv_name.
        WHEN '71'.
          CONCATENATE rv_name 'q' INTO rv_name.
        WHEN '72'.
          CONCATENATE rv_name 'r' INTO rv_name.
        WHEN '73'.
          CONCATENATE rv_name 's' INTO rv_name.
        WHEN '74'.
          CONCATENATE rv_name 't' INTO rv_name.
        WHEN '75'.
          CONCATENATE rv_name 'u' INTO rv_name.
        WHEN '76'.
          CONCATENATE rv_name 'v' INTO rv_name.
        WHEN '77'.
          CONCATENATE rv_name 'w' INTO rv_name.
        WHEN '78'.
          CONCATENATE rv_name 'x' INTO rv_name.
        WHEN '79'.
          CONCATENATE rv_name 'y' INTO rv_name.
        WHEN '7A'.
          CONCATENATE rv_name 'z' INTO rv_name.
        WHEN OTHERS.
* yea, so, the classes for code page conversion is different
* in on-prem and Steampunk. Plus also sy-abcde is deprecated in
* Steampunk :o/
* So, only very basic UTF8 conversion is implemented
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
