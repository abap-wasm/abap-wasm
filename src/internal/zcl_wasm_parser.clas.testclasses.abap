
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_wasm_parser.

    METHODS:
      setup,
      add_two FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD add_two.

* (module
*   (func $add (param $lhs i32) (param $rhs i32) (result i32)
*     get_local $lhs
*     get_local $rhs
*     i32.add)
*   (export "add" (func $add))
* )

    DATA lv_xstr TYPE xstring.

    lv_xstr = |0061736D0100000001070160027F7F01| &&
      |7F030201000707010361646400000A09| &&
      |010700200020016A0B001C046E616D65| &&
      |0106010003616464020D01000200036C| &&
      |68730103726873|.



  ENDMETHOD.

ENDCLASS.
