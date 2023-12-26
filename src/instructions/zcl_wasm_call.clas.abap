CLASS zcl_wasm_call DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_funcidx TYPE i.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.

  PRIVATE SECTION.
    DATA mv_funcidx TYPE i.
ENDCLASS.

CLASS zcl_wasm_call IMPLEMENTATION.

  METHOD constructor.
    mv_funcidx = iv_funcidx.
  ENDMETHOD.

  METHOD parse.
* todo: singletons?

    ri_instruction = NEW zcl_wasm_call( io_body->shift_int( ) ).

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions

* The call instruction invokes another function, consuming the necessary arguments from the stack
* and returning the result values of the call

    DATA(ls_type) = io_module->get_type_by_index( mv_funcidx ).
    DATA(ls_code) = io_module->get_code_by_index( mv_funcidx ).

* consume values from stack
    DATA(lo_memory) = NEW zcl_wasm_memory( ).
    DO xstrlen( ls_type-parameter_types ) TIMES.
      lo_memory->local_push( io_memory->stack_pop( ) ).
    ENDDO.

    NEW zcl_wasm_vm(
      io_memory = lo_memory
      io_module = io_module )->execute2( ls_code-instructions2 ).

* return to stack
    DO xstrlen( ls_type-result_types ) TIMES.
      io_memory->stack_push( lo_memory->stack_pop( ) ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.