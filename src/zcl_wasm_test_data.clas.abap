CLASS zcl_wasm_test_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS wasm_add_two
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS wasm_fibonacci
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS wasm_factorial
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_test_data IMPLEMENTATION.


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
      |0A09| && |010700| && |200020016A0B| && " code
      |001C| && |046E616D650106010003616464020D01000200036C68730103726873|.

  ENDMETHOD.


  METHOD wasm_fibonacci.

*(module
*  (type $t0 (func (param i32) (result i32)))
*  (func $fib (export "fib") (type $t0) (param $n i32) (result i32)
*    get_local $n
*    i32.const 2
*    i32.lt_s
*    if $I0
*      i32.const 1
*      return
*    end
*    get_local $n
*    i32.const 2
*    i32.sub
*    call $fib
*    get_local $n
*    i32.const 1
*    i32.sub
*    call $fib
*    i32.add
*    return))

    rv_xstr =
      |0061736D| && " magic
      |01000000| && " version
      |0106| && |0160017F017F| && " types
      |0302| && |0100| && " function
      |0707| && |01036669620000| && " export
      |0A1F| && |011D00| && " Start code section
        |2000| && " get local '00'
        |4102| && " const '02'
        |48| &&   " lt_s
        |0440| && " IF, blocktype = '40'
        |4101| && " const '01'
        |0F| &&   " return
        |0B| &&   " block end
        |2000| && " get local '00'
        |4102| && " const '02'
        |6B| &&   " subtract
        |1000| && " call '00'
        |2000| && " get local '00'
        |41016B10006A0F0B| &&
      |0015| && |046E616D650106010003666962020601000100016E|.

  ENDMETHOD.

  METHOD wasm_factorial.

    " (module
    " (func $fac (export "fac") (param i32) (result i32)
    "   local.get 0
    "   i32.const 1
    "   i32.lt_s
    "   if (result i32)
    "     i32.const 1
    "   else
    "     local.get 0
    "     local.get 0
    "     i32.const 1
    "     i32.sub
    "     call $fac
    "     i32.mul
    "   end))

    rv_xstr =
      |0061736D| && " magic
      |01000000| && " version
      |0106| && |0160017F017F| && " types
      |0302| && |0100| && " function
      |0707| && |01036661630000| && " export
      |0A19| && |011700| &&
      |2000| && " get local '00'
      |4101| && " const '01'
      |48| && " i32_lt_s
      |047F4101052000200041016B10006C0B0B0012046E616D6501060100036661630203010000|.

  ENDMETHOD.

ENDCLASS.
