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
  PROTECTED SECTION.

    DATA mo_memory TYPE REF TO zcl_wasm_memory .
    DATA mo_instructions TYPE REF TO zcl_wasm_binary_stream .
    DATA mo_module TYPE REF TO zcl_wasm_module .
  PRIVATE SECTION.

    METHODS if_ .
    METHODS execute
      IMPORTING
        !iv_instructions TYPE xstring .
ENDCLASS.



CLASS ZCL_WASM_VM IMPLEMENTATION.


  METHOD call.

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

    mo_instructions = NEW zcl_wasm_binary_stream( iv_instructions ).

    WHILE mo_instructions->get_length( ) > 0.
      DATA(lv_instruction) = mo_instructions->shift( 1 ).
      CASE lv_instruction.
        WHEN zcl_wasm_instructions=>c_instructions-local_get.
          zcl_wasm_local=>get( io_memory = mo_memory
                               iv_index  = mo_instructions->shift_int( ) ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_add.
          zcl_wasm_i32=>add( mo_memory ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_sub.
          zcl_wasm_i32=>sub( mo_memory ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_const.
          zcl_wasm_i32=>const_( io_memory = mo_memory
                                iv_value  = mo_instructions->shift_int( ) ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_lt_s.
          zcl_wasm_i32=>lt_s( mo_memory ).
        WHEN zcl_wasm_instructions=>c_instructions-call.
          call( mo_instructions->shift_int( ) ).
        WHEN zcl_wasm_instructions=>c_instructions-if_.
          if_( ).
        WHEN zcl_wasm_instructions=>c_instructions-return_.
          RETURN.
        WHEN zcl_wasm_instructions=>c_instructions-unreachable.
          ASSERT 0 = 1.
        WHEN zcl_wasm_instructions=>c_instructions-end.
* nothing
        WHEN OTHERS.
* todo, to be implemented
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD if_.

* https://webassembly.github.io/spec/core/exec/instructions.html#control-instructions

* hex '40' = empty block type
    ASSERT mo_instructions->shift( 1 ) = '40'.

* If c is non-zero, then enter
    DATA(lv_value) = mo_memory->stack_pop_i32( )->get_value( ).
    IF lv_value <> 0.
      RETURN.
    ENDIF.

* else forward instrcutions to '0B', this is a wrong implementation, but will work for now
    WHILE mo_instructions->peek( 1 ) <> '0B'.
      mo_instructions->shift( 1 ).
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
