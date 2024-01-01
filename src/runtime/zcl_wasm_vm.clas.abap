CLASS zcl_wasm_vm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory
        !io_module TYPE REF TO zcl_wasm_module .

    METHODS call
      IMPORTING
        !iv_index TYPE i .

    METHODS execute
      IMPORTING
        !it_instructions TYPE zif_wasm_instruction=>ty_list .

  PROTECTED SECTION.

    DATA mo_memory TYPE REF TO zcl_wasm_memory .
    DATA mo_instructions TYPE REF TO zcl_wasm_binary_stream .
    DATA mo_module TYPE REF TO zcl_wasm_module .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_wasm_vm IMPLEMENTATION.


  METHOD call.
* todo: consolidate some of this code with ZCL_WASM_CALL ?

* https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions

* The call instruction invokes another function, consuming the necessary arguments from the stack
* and returning the result values of the call

    DATA(ls_type) = mo_module->get_type_by_index( iv_index ).
    DATA(ls_code) = mo_module->get_code_by_index( iv_index ).

* consume values from stack
    DATA(lo_memory) = NEW zcl_wasm_memory( ).
    DO xstrlen( ls_type-parameter_types ) TIMES.
      lo_memory->local_push( mo_memory->stack_pop( ) ).
    ENDDO.

    NEW zcl_wasm_vm(
      io_memory = lo_memory
      io_module = mo_module )->execute( ls_code-instructions ).

* return to stack
    DO xstrlen( ls_type-result_types ) TIMES.
      mo_memory->stack_push( lo_memory->stack_pop( ) ).
    ENDDO.

  ENDMETHOD.


  METHOD constructor.
    mo_memory = io_memory.
    mo_module = io_module.
  ENDMETHOD.


  METHOD execute.

    LOOP AT it_instructions INTO DATA(lo_instruction).
      DATA(lv_control) = lo_instruction->execute(
        io_memory = mo_memory
        io_module = mo_module ).
      IF lv_control = zif_wasm_instruction=>c_control-return_.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
