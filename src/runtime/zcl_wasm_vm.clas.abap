CLASS zcl_wasm_vm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
        !io_module TYPE REF TO zcl_wasm_module .

    METHODS execute
      IMPORTING
        !it_instructions TYPE zif_wasm_instruction=>ty_list
      RETURNING
        VALUE(rv_control) TYPE zif_wasm_instruction=>ty_control
      RAISING
        zcx_wasm
        zcx_wasm_branch.

  PROTECTED SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory .
    DATA mo_module TYPE REF TO zcl_wasm_module .

ENDCLASS.



CLASS zcl_wasm_vm IMPLEMENTATION.

  METHOD constructor.
    mo_memory = io_memory.
    mo_module = io_module.
  ENDMETHOD.

  METHOD execute.
* move this method insice the module class?
    LOOP AT it_instructions INTO DATA(lo_instruction).
*      WRITE / '@KERNEL console.dir(lo_instruction.get().constructor.name);'.
      rv_control = lo_instruction->execute(
        io_memory = mo_memory
        io_module = mo_module ).

      IF rv_control = zif_wasm_instruction=>c_control-return_.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
