CLASS zcl_wasm_call DEFINITION PUBLIC.
  PUBLIC SECTION.

    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        !iv_funcidx TYPE int8.

    CLASS-METHODS parse
      IMPORTING
        !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.

    CLASS-METHODS invoke
      IMPORTING
        iv_funcidx TYPE int8
        io_memory  TYPE REF TO zcl_wasm_memory
        io_module  TYPE REF TO zcl_wasm_module
      RAISING
        zcx_wasm.

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

  METHOD invoke.
* https://webassembly.github.io/spec/core/exec/instructions.html#exec-invoke

    DATA lt_parameters TYPE zif_wasm_value=>ty_values.
    DATA lt_results    TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH EMPTY KEY.
    DATA ls_control    TYPE zif_wasm_instruction=>ty_control.

    DATA(ls_function) = io_module->get_function_by_index( iv_funcidx ).
*    WRITE: / |call funcidx { iv_funcidx }|.
    DATA(ls_type) = io_module->get_type_by_index( CONV #( ls_function-typeidx ) ).

    IF ls_function-extern_module IS NOT INITIAL.
      DATA(li_module) = io_module->get_import_by_module_name( ls_function-extern_module ).

      DO xstrlen( ls_type-parameter_types ) TIMES.
* todo: check parameters types are correct
        INSERT io_memory->mi_stack->pop( ) INTO TABLE lt_parameters.
      ENDDO.

      DATA(lt_result) = li_module->execute_function_export(
        iv_name       = ls_function-extern_name
        it_parameters = lt_parameters ).

* todo: check results match expected
      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<li_result>).
        io_memory->mi_stack->push( <li_result> ).
      ENDLOOP.
    ELSE.
      DATA(lr_code) = io_module->get_code_by_index( CONV #( ls_function-codeidx ) ).

* consume values from stack into locals
      io_memory->push_locals( ).
      DO xstrlen( ls_type-parameter_types ) TIMES.
* todo: check parameters types are correct
        INSERT io_memory->mi_stack->pop( ) INTO io_memory->mt_locals INDEX 1.
      ENDDO.

* add the locals for the function
      LOOP AT lr_code->locals ASSIGNING FIELD-SYMBOL(<ls_local>).
        DO <ls_local>-count TIMES.
          CASE <ls_local>-type.
            WHEN zif_wasm_types=>c_value_type-i32.
              INSERT NEW zcl_wasm_i32( ) INTO TABLE io_memory->mt_locals.
            WHEN zif_wasm_types=>c_value_type-i64.
              INSERT NEW zcl_wasm_i64( ) INTO TABLE io_memory->mt_locals.
            WHEN zif_wasm_types=>c_value_type-f32.
              INSERT NEW zcl_wasm_f32( ) INTO TABLE io_memory->mt_locals.
            WHEN zif_wasm_types=>c_value_type-f64.
              INSERT NEW zcl_wasm_f64( ) INTO TABLE io_memory->mt_locals.
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |call: unknown type|.
          ENDCASE.
        ENDDO.
      ENDLOOP.

      DATA(li_old_stack) = io_memory->mi_stack.
      io_memory->mi_stack = CAST zif_wasm_memory_stack( NEW zcl_wasm_memory_stack( ) ).

      io_module->execute_instructions(
            EXPORTING
              it_instructions = lr_code->instructions
            CHANGING
              cs_control      = ls_control ).
      IF ls_control-depth > 0.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'call(), branching, should not happen'.
      ENDIF.

******************

      "##feature-start=debug
      IF xstrlen( ls_type-result_types ) > io_memory->mi_stack->get_length( ).
        RAISE EXCEPTION TYPE zcx_wasm
          EXPORTING
            text = |call: too few results got { io_memory->mi_stack->get_length( ) } expected at least { xstrlen( ls_type-result_types ) }|.
      ENDIF.
      "##feature-end=debug

      DO xstrlen( ls_type-result_types ) TIMES.
        DATA(lv_offset) = xstrlen( ls_type-result_types ) - sy-index.
        DATA(li_val) = io_memory->mi_stack->pop( ).

        "##feature-start=debug
        IF li_val->get_type( ) <> ls_type-result_types+lv_offset(1).
          RAISE EXCEPTION TYPE zcx_wasm
            EXPORTING
              text = |call result: wrong parameter on stack, got { li_val->get_type( ) } expected { ls_type-result_types+lv_offset(1) }|.
        ENDIF.
        "##feature-end=debug

        INSERT li_val INTO lt_results INDEX 1.
      ENDDO.

      LOOP AT lt_results INTO li_val.
        li_old_stack->push( li_val ).
      ENDLOOP.

      io_memory->mi_stack = li_old_stack.
      io_memory->pop_locals( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
* https://webassembly.github.io/spec/core/exec/instructions.html#exec-invoke

* The call instruction invokes another function, consuming the necessary arguments from the stack
* and returning the result values of the call

    invoke(
      iv_funcidx = mv_funcidx
      io_memory  = io_memory
      io_module  = io_module ).

  ENDMETHOD.

ENDCLASS.
