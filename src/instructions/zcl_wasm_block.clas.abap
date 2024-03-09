CLASS zcl_wasm_block DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    METHODS constructor
      IMPORTING
        iv_block_type   TYPE xstring
        it_instructions TYPE zif_wasm_instruction=>ty_list.

    CLASS-METHODS parse
      IMPORTING
        io_body TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction
      RAISING
        zcx_wasm.

    CLASS-METHODS fix_return
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory
        io_module TYPE REF TO zcl_wasm_module
        iv_block_type TYPE xstring
        iv_length TYPE i
      RAISING
        zcx_wasm.

  PRIVATE SECTION.
    DATA mv_block_type   TYPE xstring.
    DATA mt_instructions TYPE zif_wasm_instruction=>ty_list.

ENDCLASS.

CLASS zcl_wasm_block IMPLEMENTATION.

  METHOD constructor.
* https://webassembly.github.io/spec/core/valid/types.html#block-types
    mv_block_type   = iv_block_type.
    mt_instructions = it_instructions.
  ENDMETHOD.

  METHOD parse.

* Block types are encoded in special compressed form, by either the byte
* indicating the empty type, as a single value type, or as a type index
* encoded as a positive signed integer.
    DATA(lv_block_type) = io_body->shift( 1 ).

    zcl_wasm_instructions=>parse(
      EXPORTING
        io_body         = io_body
      IMPORTING
        ev_last_opcode  = DATA(lv_last_opcode)
        et_instructions = DATA(lt_instructions) ).

    IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |block, expected end|.
    ENDIF.

    ri_instruction = NEW zcl_wasm_block(
      iv_block_type   = lv_block_type
      it_instructions = lt_instructions ).

  ENDMETHOD.

  METHOD fix_return.

    DATA lv_return  TYPE xstring.
    DATA lv_int8    TYPE int8.
    DATA lt_results TYPE STANDARD TABLE OF REF TO zif_wasm_value WITH EMPTY KEY.

    CASE iv_block_type.
      WHEN zcl_wasm_types=>c_empty_block_type.
        RETURN.
      WHEN zcl_wasm_types=>c_value_type-i32
          OR zcl_wasm_types=>c_value_type-i64
          OR zcl_wasm_types=>c_value_type-f32
          OR zcl_wasm_types=>c_value_type-f64
          OR zcl_wasm_types=>c_reftype-funcref
          OR zcl_wasm_types=>c_reftype-externref
          OR zcl_wasm_types=>c_vector_type.
        lv_return = iv_block_type.
      WHEN OTHERS.
        lv_int8 = iv_block_type.
        IF lv_int8 < 0.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |block: expected positive function type index|.
        ENDIF.
        DATA(ls_type) = io_module->get_type_by_index( lv_int8 ).
        lv_return = ls_type-result_types.
    ENDCASE.

    IF xstrlen( lv_return ) > io_memory->stack_length( ).
*      WRITE '@KERNEL throw new Error("sdf");'.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |block: expected { xstrlen( lv_return ) } values on stack, { lv_return }|.
    ENDIF.
    DO xstrlen( lv_return ) TIMES.
      DATA(li_val) = io_memory->stack_pop( ).
      INSERT li_val INTO lt_results INDEX 1.
    ENDDO.

    WHILE io_memory->stack_length( ) > iv_length AND io_memory->stack_length( ) > 0.
      io_memory->stack_pop( ).
    ENDWHILE.

    LOOP AT lt_results INTO li_val.
      io_memory->stack_push( li_val ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_wasm_instruction~execute.

* https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-block-xref-syntax-instructions-syntax-blocktype-mathit-blocktype-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-control-mathsf-end

    DATA(lv_length) = io_memory->stack_length( ).

    TRY.
        rv_control = NEW zcl_wasm_vm(
          io_memory = io_memory
          io_module = io_module )->execute( mt_instructions ).
      CATCH zcx_wasm_branch INTO DATA(lx_branch).
        fix_return( io_memory     = io_memory
                    io_module     = io_module
                    iv_block_type = mv_block_type
                    iv_length     = lv_length ).
        IF lx_branch->depth > 0.
          RAISE EXCEPTION TYPE zcx_wasm_branch EXPORTING depth = lx_branch->depth - 1.
        ENDIF.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
