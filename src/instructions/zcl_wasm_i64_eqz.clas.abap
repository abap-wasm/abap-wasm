CLASS zcl_wasm_i64_eqz DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_wasm_instruction.

    CLASS-METHODS parse
      IMPORTING !io_body              TYPE REF TO zcl_wasm_binary_stream
      RETURNING VALUE(ri_instruction) TYPE REF TO zif_wasm_instruction.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gi_singleton TYPE REF TO zif_wasm_instruction.
ENDCLASS.



CLASS zcl_wasm_i64_eqz IMPLEMENTATION.


  METHOD parse.
    IF gi_singleton IS INITIAL.
      gi_singleton = NEW zcl_wasm_i64_eqz( ).
    ENDIF.
    ri_instruction = gi_singleton.
  ENDMETHOD.


  METHOD zif_wasm_instruction~execute.

    "##feature=debug
    IF io_memory->mi_stack->get_length( ) < 1.
      RAISE EXCEPTION TYPE zcx_wasm EXPORTING text = 'i64, eqz, expected value on stack'.
    ENDIF.

    DATA(lv_val1) = CAST zcl_wasm_i64( io_memory->mi_stack->pop( ) )->get_signed( ).

    IF lv_val1 = 0.
      io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( 1 ) ).
    ELSE.
      io_memory->mi_stack->push( zcl_wasm_i32=>from_signed( 0 ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
