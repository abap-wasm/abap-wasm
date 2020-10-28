CLASS zcl_wasm_types DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_type TYPE x LENGTH 1 .

* https://webassembly.github.io/spec/core/binary/types.html#value-types
    CONSTANTS:
      BEGIN OF c_value_type,
        i32 TYPE ty_type VALUE '7F',
        i64 TYPE ty_type VALUE '7E',
        f32 TYPE ty_type VALUE '7D',
        f64 TYPE ty_type VALUE '7C',
      END OF c_value_type .

* https://webassembly.github.io/spec/core/binary/types.html#function-types
    CONSTANTS c_function_type TYPE ty_type VALUE '60'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_TYPES IMPLEMENTATION.
ENDCLASS.
