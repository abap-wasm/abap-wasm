CLASS zcl_wasm_value_types DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES ty_value_type TYPE x LENGTH 1.

    CONSTANTS: BEGIN OF c_type,
                 i32 TYPE ty_value_type VALUE '7F',
                 i64 TYPE ty_value_type VALUE '7E',
                 f32 TYPE ty_value_type VALUE '7D',
                 f64 TYPE ty_value_type VALUE '7C',
               END OF c_type.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_VALUE_TYPES IMPLEMENTATION.
ENDCLASS.
