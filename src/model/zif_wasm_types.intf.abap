INTERFACE zif_wasm_types PUBLIC.

  TYPES ty_type TYPE x LENGTH 1 .
  TYPES ty_valtype TYPE x LENGTH 1 .

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

  CONSTANTS:
      BEGIN OF c_limit,
        min TYPE x LENGTH 1 VALUE '00',
        max TYPE x LENGTH 1 VALUE '01',
      END OF c_limit.

  TYPES: BEGIN OF ty_limit,
             min TYPE i,
             max TYPE i,
           END OF ty_limit.

  TYPES: BEGIN OF ty_import,
             name   TYPE string,
             module TYPE REF TO zif_wasm_module,
           END OF ty_import.
  TYPES ty_imports_tt TYPE STANDARD TABLE OF ty_import WITH DEFAULT KEY.

ENDINTERFACE.
