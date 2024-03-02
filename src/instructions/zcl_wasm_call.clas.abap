CLASS zcl_wasm_call DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_funcidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING !io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING zcx_wasm.

  PRIVATE SECTION.
    DATA mv_funcidx TYPE int8.
ENDCLASS.

CLASS zcl_wasm_call IMPLEMENTATION.

  METHOD constructor.
    mv_funcidx = iv_funcidx.
  ENDMETHOD.

  METHOD parse.
    ri_instruction = NEW zcl_wasm_call( io_body->shift_u32( ) ).
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
* https://webassembly.github.io/spec/core/exec/instructions.html#exec-invoke

* The call instruction invokes another function, consuming the necessary arguments from the stack
* and returning the result values of the call

    DATA(ls_function) = io_module->get_function_by_index( mv_funcidx ).
    DATA(ls_type) = io_module->get_type_by_index( CONV #( ls_function-typeidx ) ).
    DATA(ls_code) = io_module->get_code_by_index( CONV #( ls_function-codeidx ) ).

* consume values from stack into locals
    io_memory->push_frame( ).
    DO xstrlen( ls_type-parameter_types ) TIMES.
      io_memory->get_frame( )->local_push_first( io_memory->stack_pop( ) ).
    ENDDO.

* add the locals for the function
    LOOP AT ls_code-locals INTO DATA(ls_local).
      DO ls_local-count TIMES.
        CASE ls_local-type.
          WHEN zcl_wasm_types=>c_value_type-i32.
            io_memory->get_frame( )->local_push_last( NEW zcl_wasm_i32( ) ).
          WHEN zcl_wasm_types=>c_value_type-i64.
            io_memory->get_frame( )->local_push_last( NEW zcl_wasm_i64( ) ).
          WHEN zcl_wasm_types=>c_value_type-f32.
            io_memory->get_frame( )->local_push_last( NEW zcl_wasm_f32( ) ).
          WHEN zcl_wasm_types=>c_value_type-f64.
            io_memory->get_frame( )->local_push_last( NEW zcl_wasm_f64( ) ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |call: unknown type|.
        ENDCASE.
      ENDDO.
    ENDLOOP.

    NEW zcl_wasm_vm(
      io_memory = io_memory
      io_module = io_module )->execute( ls_code-instructions ).

    io_memory->pop_frame( ).

  ENDMETHOD.

ENDCLASS.
