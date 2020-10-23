CLASS zcl_wasm_instructions DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES ty_instruction TYPE x LENGTH 1.
    TYPES ty_instructions TYPE STANDARD TABLE OF ty_instruction WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_instructions,
* https://webassembly.github.io/spec/core/binary/instructions.html#parametric-instructions
        drop       TYPE ty_instruction VALUE '1A',
        select     TYPE ty_instruction VALUE '1B',
* https://webassembly.github.io/spec/core/binary/instructions.html#variable-instructions
        local_get  TYPE ty_instruction VALUE '20',
        local_set  TYPE ty_instruction VALUE '21',
        local_tee  TYPE ty_instruction VALUE '22',
        global_get TYPE ty_instruction VALUE '23',
        global_set TYPE ty_instruction VALUE '24',
      END OF c_instructions.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WASM_INSTRUCTIONS IMPLEMENTATION.
ENDCLASS.
