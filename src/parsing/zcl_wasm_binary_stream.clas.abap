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
    METHODS shift_utf8
      RETURNING
        VALUE(rv_name) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring .
ENDCLASS.



CLASS ZCL_WASM_BINARY_STREAM IMPLEMENTATION.


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
* https://webassembly.github.io/spec/core/binary/values.html#binary-int

    DATA lv_hex TYPE x LENGTH 1.
    lv_hex = shift( 1 ).
    rv_int = CONV i( lv_hex ).

  ENDMETHOD.


  METHOD shift_utf8.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( shift( shift_int( ) ) ).

    WHILE lo_stream->get_length( ) > 0.
      DATA(lv_hex) = lo_stream->shift( 1 ).
      CASE lv_hex.
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
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
