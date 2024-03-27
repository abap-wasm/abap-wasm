CLASS zcl_wasm_global_section DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS parse
      IMPORTING
        !io_body         TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(ro_global) TYPE REF TO zcl_wasm_global_section
      RAISING
        zcx_wasm.

    TYPES:
      BEGIN OF ty_global,
        type         TYPE x LENGTH 1,
        mut          TYPE x LENGTH 1,
        instructions TYPE zif_wasm_instruction=>ty_list,
      END OF ty_global.
    TYPES: ty_globals TYPE STANDARD TABLE OF ty_global WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        it_globals TYPE ty_globals OPTIONAL.

    METHODS instantiate
      IMPORTING
        io_memory TYPE REF TO zcl_wasm_memory
      RAISING
        zcx_wasm.

    CONSTANTS: BEGIN OF c_mut,
                 const TYPE x VALUE '00',
                 var   TYPE x VALUE '01',
               END OF c_mut.

  PRIVATE SECTION.
    DATA mt_globals TYPE ty_globals.
ENDCLASS.

CLASS zcl_wasm_global_section IMPLEMENTATION.

  METHOD constructor.
    mt_globals = it_globals.
  ENDMETHOD.

  METHOD instantiate.

    DATA ls_control TYPE zif_wasm_instruction=>ty_control.

    LOOP AT mt_globals INTO DATA(ls_global).
      TRY.
          LOOP AT ls_global-instructions INTO DATA(lo_instruction).
            lo_instruction->execute(
              EXPORTING
                io_memory  = io_memory
                io_module  = NEW zcl_wasm_module( )
              CHANGING
                cs_control = ls_control ).
          ENDLOOP.
        CATCH cx_static_check INTO DATA(lx_error).
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate_global, failed to execute instructions: { lx_error->get_text( ) }|.
      ENDTRY.

      DATA(li_value) = io_memory->mi_stack->pop( ).
      IF li_value IS INITIAL.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate_global, initial value on stack|.
      ENDIF.

      CASE ls_global-type.
        WHEN zif_wasm_types=>c_value_type-i32
            OR zif_wasm_types=>c_value_type-i64
            OR zif_wasm_types=>c_value_type-f32
            OR zif_wasm_types=>c_value_type-f64
            OR zif_wasm_types=>c_reftype-funcref
            OR zif_wasm_types=>c_reftype-externref.
          IF li_value->get_type( ) <> ls_global-type.
            RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate_global, type mismatch: { ls_global-type } vs { li_value->get_type( ) }|.
          ENDIF.
          io_memory->get_globals( )->append( li_value ).
        WHEN zif_wasm_types=>c_vector_type.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate_global, todo vector type|.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate_global, unknown type { ls_global-type }|.
      ENDCASE.

      CASE ls_global-mut.
        WHEN c_mut-const.
* todo
        WHEN c_mut-var.
* todo
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |instantiate_global, unknown mut|.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD parse.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-globalsec

    DATA lt_globals TYPE ty_globals.
    DATA ls_global  LIKE LINE OF lt_globals.

    DATA(lv_times) = io_body->shift_u32( ).
    DO lv_times TIMES.
      CLEAR ls_global.
      ls_global-type = io_body->shift( 1 ).
      ls_global-mut = io_body->shift( 1 ).

      zcl_wasm_instructions=>parse(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = DATA(lv_last_opcode)
          et_instructions = ls_global-instructions ).
      IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
        RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = |parse_global, expected end|.
      ENDIF.
      INSERT ls_global INTO TABLE lt_globals.
    ENDDO.

    ro_global = NEW zcl_wasm_global_section( lt_globals ).

  ENDMETHOD.

ENDCLASS.
