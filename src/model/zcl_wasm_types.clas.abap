CLASS zcl_wasm_types DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_type TYPE x LENGTH 1 .
    TYPES:
      ty_valtype TYPE x LENGTH 1 .

* https://webassembly.github.io/spec/core/binary/types.html#value-types
    CONSTANTS:
      BEGIN OF c_value_type,
        i32 TYPE ty_type VALUE '7F',
        i64 TYPE ty_type VALUE '7E',
        f32 TYPE ty_type VALUE '7D',
        f64 TYPE ty_type VALUE '7C',
      END OF c_value_type .

    CONSTANTS c_vector_type TYPE ty_type VALUE '7B'.

    CONSTANTS:
      BEGIN OF c_reftype,
        funcref   TYPE ty_type VALUE '70',
        externref TYPE ty_type VALUE '6F',
      END OF c_reftype.

* https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
    CONSTANTS c_empty_block_type TYPE ty_type VALUE '40'.

* https://webassembly.github.io/spec/core/binary/types.html#function-types
    CONSTANTS c_function_type TYPE ty_type VALUE '60'.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-exportsec
    CONSTANTS:
      BEGIN OF c_export_type,
        func   TYPE ty_type VALUE '00',
        table  TYPE ty_type VALUE '01',
        mem    TYPE ty_type VALUE '02',
        global TYPE ty_type VALUE '03',
      END OF c_export_type .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_types IMPLEMENTATION.
ENDCLASS.
