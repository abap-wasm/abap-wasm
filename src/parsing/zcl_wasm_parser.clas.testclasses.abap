
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

    DATA(lv_xstr) = CONV xstring(
      |0061736D| && " magic
      |01000000| && " version
      |0107| && |0160027F7F017F| && " types
      |0302| && |0100| &&  " function
      |0707| && |01036164640000| && " export
      |0A09| && |010700200020016A0B| && " code
      |001C| && |046E616D650106010003616464020D01000200036C68730103726873| ).

    mo_cut->parse( lv_xstr ).

  ENDMETHOD.

ENDCLASS.
