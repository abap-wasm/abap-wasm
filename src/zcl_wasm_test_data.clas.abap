CLASS zcl_wasm_test_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS wasm_add_two
      RETURNING
        VALUE(rv_xstr) TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_TEST_DATA IMPLEMENTATION.


  METHOD wasm_add_two.

* (module
*   (func $add (param $lhs i32) (param $rhs i32) (result i32)
*     get_local $lhs
*     get_local $rhs
*     i32.add)
*   (export "add" (func $add))
* )

    rv_xstr =
      |0061736D| && " magic
      |01000000| && " version
      |0107| && |0160027F7F017F| && " types
      |0302| && |0100| &&  " function
      |0707| && |01036164640000| && " export
      |0A09| && |010700200020016A0B| && " code
      |001C| && |046E616D650106010003616464020D01000200036C68730103726873|.

  ENDMETHOD.
ENDCLASS.