CLASS zcl_wasm_vm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_memory TYPE REF TO zcl_wasm_memory.

    METHODS execute
      IMPORTING
        iv_instructions TYPE xstring.
  PROTECTED SECTION.
    DATA mo_memory TYPE REF TO zcl_wasm_memory.
  PRIVATE SECTION.

    METHODS if_ .
ENDCLASS.



CLASS ZCL_WASM_VM IMPLEMENTATION.


  METHOD constructor.
    mo_memory = io_memory.
  ENDMETHOD.


  METHOD execute.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_instructions ).

    WHILE lo_stream->get_length( ) > 0.
      DATA(lv_instruction) = lo_stream->shift( 1 ).
      CASE lv_instruction.
        WHEN zcl_wasm_instructions=>c_instructions-local_get.
          zcl_wasm_local=>get( io_memory = mo_memory
                               iv_index  = lo_stream->shift_int( ) ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_add.
          zcl_wasm_i32=>add( mo_memory ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_const.
          zcl_wasm_i32=>const_( io_memory = mo_memory
                                iv_value  = lo_stream->shift_int( ) ).
        WHEN zcl_wasm_instructions=>c_instructions-i32_lt_s.
          zcl_wasm_i32=>lt_s( mo_memory ).
        WHEN zcl_wasm_instructions=>c_instructions-call.
          ASSERT 0 = 1. " todo
        WHEN zcl_wasm_instructions=>c_instructions-if_.
          if_( ).
        WHEN zcl_wasm_instructions=>c_instructions-return_.
          ASSERT 0 = 1. " todo
        WHEN zcl_wasm_instructions=>c_instructions-unreachable.
* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-unreachable
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

* If c is non-zero, then enter
    DATA(lv_value) = mo_memory->stack_pop_i32( )->get_value( ).

* hex '40' = empty block type

* todo

  ENDMETHOD.
ENDCLASS.
