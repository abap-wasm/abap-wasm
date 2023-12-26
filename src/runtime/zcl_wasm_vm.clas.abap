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

    METHODS execute2
      IMPORTING
        !it_instructions TYPE zif_wasm_instruction=>ty_list .
ENDCLASS.



CLASS zcl_wasm_vm IMPLEMENTATION.


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
      io_module = mo_module )->execute2( ls_code-instructions2 ).

* return to stack
    DO xstrlen( ls_type-result_types ) TIMES.
      mo_memory->stack_push( lo_memory->stack_pop( ) ).
    ENDDO.

  ENDMETHOD.


  METHOD constructor.
    mo_memory = io_memory.
    mo_module = io_module.
  ENDMETHOD.

  METHOD execute2.

    LOOP AT it_instructions INTO DATA(lo_instruction).
      lo_instruction->execute(
        io_memory = mo_memory
        io_module = mo_module ).
    ENDLOOP.

  ENDMETHOD.

  METHOD execute.

    mo_instructions = NEW zcl_wasm_binary_stream( iv_instructions ).

    WHILE mo_instructions->get_length( ) > 0.
      DATA(lv_instruction) = mo_instructions->shift( 1 ).
      " WRITE: / 'instruction:', lv_instruction.
      CASE lv_instruction.
        WHEN zif_wasm_opcodes=>c_opcodes-local_get.
          CAST zif_wasm_instruction( NEW zcl_wasm_local_get( mo_instructions->shift_int( ) ) )->execute(
            io_memory = mo_memory
            io_module = mo_module ).
        WHEN zif_wasm_opcodes=>c_opcodes-i32_add.
          CAST zif_wasm_instruction( NEW zcl_wasm_i32_add( ) )->execute(
            io_memory = mo_memory
            io_module = mo_module ).
        WHEN zif_wasm_opcodes=>c_opcodes-i32_sub.
          CAST zif_wasm_instruction( NEW zcl_wasm_i32_sub( ) )->execute(
            io_memory = mo_memory
            io_module = mo_module ).
        WHEN zif_wasm_opcodes=>c_opcodes-i32_const.
          CAST zif_wasm_instruction( NEW zcl_wasm_i32_const( mo_instructions->shift_int( ) ) )->execute(
            io_memory = mo_memory
            io_module = mo_module ).
        WHEN zif_wasm_opcodes=>c_opcodes-i32_lt_s.
          CAST zif_wasm_instruction( NEW zcl_wasm_i32_lt_s( ) )->execute(
            io_memory = mo_memory
            io_module = mo_module ).
        WHEN zif_wasm_opcodes=>c_opcodes-call.
          call( mo_instructions->shift_int( ) ).
        WHEN zif_wasm_opcodes=>c_opcodes-if_.
          if_( ).
        WHEN zif_wasm_opcodes=>c_opcodes-return_.
          RETURN.
        WHEN zif_wasm_opcodes=>c_opcodes-unreachable.
          ASSERT 0 = 1.
        WHEN zif_wasm_opcodes=>c_opcodes-end.
* nothing
        WHEN OTHERS.
* todo, to be implemented
          ASSERT 0 = 1.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD if_.

* https://webassembly.github.io/spec/core/exec/instructions.html#control-instructions
* https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
* https://webassembly.github.io/spec/core/binary/instructions.html#binary-blocktype

    DATA(lv_block_type) = mo_instructions->shift( 1 ).
*    WRITE: / 'if:', lv_block_type.

    CASE lv_block_type.
      WHEN '40'.
* hex '40' = empty block type

* If c is non-zero, then enter
        DATA(lv_value) = mo_memory->stack_pop_i32( )->get_value( ).
        IF lv_value <> 0.
          RETURN.
        ENDIF.

* else forward instructions to '0B', this is a wrong implementation, but will work for now
        WHILE mo_instructions->peek( 1 ) <> '0B'.
          mo_instructions->shift( 1 ).
        ENDWHILE.
      WHEN zcl_wasm_types=>c_value_type-i32.
        ASSERT 1 = 'todo'.
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
