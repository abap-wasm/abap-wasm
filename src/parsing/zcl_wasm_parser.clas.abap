CLASS zcl_wasm_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !iv_wasm         TYPE xstring
      RETURNING
        VALUE(ro_module) TYPE REF TO zcl_wasm_module
      RAISING
        zcx_wasm.

    METHODS parse_instructions
      IMPORTING
        !io_body        TYPE REF TO zcl_wasm_binary_stream
      EXPORTING
        ev_last_opcode  TYPE zif_wasm_opcodes=>ty_opcode
        et_instructions TYPE zif_wasm_instruction=>ty_list
      RAISING
        zcx_wasm.
  PROTECTED SECTION.

    CONSTANTS:
* Note that these constants are not structured as they contain JS keywords
      gc_section_custom   TYPE x LENGTH 1 VALUE '00' ##NO_TEXT.
    CONSTANTS:
      gc_section_type     TYPE x LENGTH 1 VALUE '01' ##NO_TEXT.
    CONSTANTS:
      gc_section_import   TYPE x LENGTH 1 VALUE '02' ##NO_TEXT.
    CONSTANTS:
      gc_section_function TYPE x LENGTH 1 VALUE '03' ##NO_TEXT.
    CONSTANTS:
      gc_section_table    TYPE x LENGTH 1 VALUE '04' ##NO_TEXT.
    CONSTANTS:
      gc_section_memory   TYPE x LENGTH 1 VALUE '05' ##NO_TEXT.
    CONSTANTS:
      gc_section_global   TYPE x LENGTH 1 VALUE '06' ##NO_TEXT.
    CONSTANTS:
      gc_section_export   TYPE x LENGTH 1 VALUE '07' ##NO_TEXT.
    CONSTANTS:
      gc_section_start    TYPE x LENGTH 1 VALUE '08' ##NO_TEXT.
    CONSTANTS:
      gc_section_element  TYPE x LENGTH 1 VALUE '09' ##NO_TEXT.
    CONSTANTS:
      gc_section_code     TYPE x LENGTH 1 VALUE '0A' ##NO_TEXT.
    CONSTANTS:
      gc_section_data     TYPE x LENGTH 1 VALUE '0B' ##NO_TEXT.
    CONSTANTS:
      gc_section_data_count TYPE x LENGTH 1 VALUE '0C' ##NO_TEXT.

    METHODS parse_code
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zcl_wasm_module=>ty_codes
      RAISING
        zcx_wasm.

    METHODS parse_function
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RETURNING
        VALUE(rt_results) TYPE zcl_wasm_module=>ty_functions
      RAISING
        zcx_wasm.

    METHODS parse_table
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.

    METHODS parse_data
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.

    METHODS parse_global
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.

    METHODS parse_element
      IMPORTING
        !io_body          TYPE REF TO zcl_wasm_binary_stream
      RAISING
        zcx_wasm.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wasm_parser IMPLEMENTATION.


  METHOD parse.

    CONSTANTS lc_magic   TYPE x LENGTH 4 VALUE '0061736D'.
    CONSTANTS lc_version TYPE x LENGTH 4 VALUE '01000000'.

    DATA(lo_stream) = NEW zcl_wasm_binary_stream( iv_wasm ).

* https://webassembly.github.io/spec/core/binary/modules.html#binary-module
    IF lo_stream->shift( 4 ) <> lc_magic.
      RAISE EXCEPTION NEW zcx_wasm( text = |unexpected magic number| ).
    ENDIF.
    IF lo_stream->shift( 4 ) <> lc_version.
      RAISE EXCEPTION NEW zcx_wasm( text = |unexpected version| ).
    ENDIF.

    WHILE lo_stream->get_length( ) > 0.
* https://webassembly.github.io/spec/core/binary/modules.html#sections
      DATA(lv_section) = lo_stream->shift( 1 ).
      DATA(lv_length) = lo_stream->shift_u32( ).
      DATA(lo_body) = NEW zcl_wasm_binary_stream( lo_stream->shift( lv_length ) ).

      " WRITE: / 'body:', lo_body->get_data( ).

      CASE lv_section.
        WHEN gc_section_custom.
* https://webassembly.github.io/spec/core/binary/modules.html#binary-customsec
* "ignored by the WebAssembly semantics"
          CONTINUE.
        WHEN gc_section_type.
          DATA(lt_types) = zcl_wasm_type_section=>parse( lo_body ).
        WHEN gc_section_import.
* todo
          zcl_wasm_import_section=>parse( lo_body ).
        WHEN gc_section_function.
          DATA(lt_functions) = parse_function( lo_body ).
        WHEN gc_section_table.
* todo
          parse_table( lo_body ).
        WHEN gc_section_memory.
* todo
          zcl_wasm_memory_section=>parse( lo_body ).
        WHEN gc_section_global.
* todo
          parse_global( lo_body ).
        WHEN gc_section_export.
          DATA(lt_exports) = zcl_wasm_export_section=>parse( lo_body ).
        WHEN gc_section_start.
* https://webassembly.github.io/spec/core/binary/modules.html#start-section
* todo
          DATA(lv_funcidx) = lo_body->shift_u32( ).
        WHEN gc_section_element.
* todo
          parse_element( lo_body ).
        WHEN gc_section_code.
          DATA(lt_codes) = parse_code( lo_body ).
        WHEN gc_section_data.
          parse_data( lo_body ).
        WHEN gc_section_data_count.
          DATA(lv_data_count) = lo_body->shift_u32( ).
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |unknown section: { lv_section }| ).
      ENDCASE.
    ENDWHILE.

    ro_module = NEW #(
      it_types     = lt_types
      it_codes     = lt_codes
      it_exports   = lt_exports
      it_functions = lt_functions ).

  ENDMETHOD.

  METHOD parse_code.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-codesec

    DATA lt_locals TYPE zcl_wasm_module=>ty_locals.
    DATA lt_instructions TYPE zif_wasm_instruction=>ty_list.

    DO io_body->shift_u32( ) TIMES.

      DATA(lv_code_size) = io_body->shift_u32( ).

      DATA(lo_code) = NEW zcl_wasm_binary_stream( io_body->shift( lv_code_size ) ).

      CLEAR lt_locals.
      DATA(lv_locals_count) = lo_code->shift_u32( ).
      DO lv_locals_count TIMES.
        DATA(lv_count) = lo_code->shift_u32( ).
        DATA(lv_locals_type) = lo_code->shift( 1 ).
        APPEND VALUE #(
          count = lv_count
          type  = lv_locals_type ) TO lt_locals.
      ENDDO.

      CLEAR lt_instructions.

      parse_instructions(
        EXPORTING io_body         = lo_code
        IMPORTING et_instructions = lt_instructions ).

      APPEND VALUE #(
        instructions = lt_instructions
        locals       = lt_locals ) TO rt_results.

    ENDDO.

  ENDMETHOD.

  METHOD parse_instructions.

    WHILE io_body->get_length( ) > 0.
      DATA(lv_opcode) = io_body->shift( 1 ).
      ev_last_opcode = lv_opcode.
      CASE lv_opcode.
        WHEN zif_wasm_opcodes=>c_opcodes-local_get.
          APPEND zcl_wasm_local_get=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-local_set.
          APPEND zcl_wasm_local_set=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-local_tee.
          APPEND zcl_wasm_local_tee=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_eqz.
          APPEND zcl_wasm_i32_eqz=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_wrap_i64.
          APPEND zcl_wasm_i32_wrap_i64=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_eq.
          APPEND zcl_wasm_i32_eq=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_ne.
          APPEND zcl_wasm_i32_ne=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_ne.
          APPEND zcl_wasm_f32_ne=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_lt_s.
          APPEND zcl_wasm_i32_lt_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_lt_u.
          APPEND zcl_wasm_i32_lt_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_store16.
          APPEND zcl_wasm_i64_store16=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_ne.
          APPEND zcl_wasm_f64_ne=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_lt.
          APPEND zcl_wasm_f64_lt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_gt.
          APPEND zcl_wasm_f64_gt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_ge.
          APPEND zcl_wasm_f64_ge=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_le.
          APPEND zcl_wasm_f64_le=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_mul.
          APPEND zcl_wasm_i64_mul=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_sub.
          APPEND zcl_wasm_f32_sub=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_eq.
          APPEND zcl_wasm_f64_eq=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_le.
          APPEND zcl_wasm_f32_le=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_sub.
          APPEND zcl_wasm_i64_sub=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_gt_s.
          APPEND zcl_wasm_i32_gt_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_le_u.
          APPEND zcl_wasm_i64_le_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_gt_u.
          APPEND zcl_wasm_i32_gt_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_le_s.
          APPEND zcl_wasm_i32_le_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_sqrt.
          APPEND zcl_wasm_f32_sqrt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_eq.
          APPEND zcl_wasm_i64_eq=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_ne.
          APPEND zcl_wasm_i64_ne=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_lt_s.
          APPEND zcl_wasm_i64_lt_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_lt_u.
          APPEND zcl_wasm_i64_lt_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_gt_s.
          APPEND zcl_wasm_i64_gt_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_gt_u.
          APPEND zcl_wasm_i64_gt_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_le_s.
          APPEND zcl_wasm_i64_le_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_ge_s.
          APPEND zcl_wasm_i64_ge_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_ge_u.
          APPEND zcl_wasm_i64_ge_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_ceil.
          APPEND zcl_wasm_f32_ceil=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_floor.
          APPEND zcl_wasm_f32_floor=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_trunc.
          APPEND zcl_wasm_f32_trunc=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_nearest.
          APPEND zcl_wasm_f32_nearest=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_eq.
          APPEND zcl_wasm_f32_eq=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_le_u.
          APPEND zcl_wasm_i32_le_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_ge_s.
          APPEND zcl_wasm_i32_ge_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_ge_u.
          APPEND zcl_wasm_i32_ge_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-block.
          APPEND zcl_wasm_block=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-nop.
          APPEND zcl_wasm_nop=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-loop.
          APPEND zcl_wasm_loop=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-br_if.
          APPEND zcl_wasm_br_if=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-global_get.
          APPEND zcl_wasm_global_get=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-global_set.
          APPEND zcl_wasm_global_set=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_const.
          APPEND zcl_wasm_f32_const=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_add.
          APPEND zcl_wasm_i32_add=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-br_table.
          APPEND zcl_wasm_br_table=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-memory_size.
          APPEND zcl_wasm_memory_size=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-memory_grow.
          APPEND zcl_wasm_memory_grow=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-call_indirect.
          APPEND zcl_wasm_call_indirect=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_store.
          APPEND zcl_wasm_i32_store=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_sub.
          APPEND zcl_wasm_i32_sub=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_ctz.
          APPEND zcl_wasm_i64_ctz=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_abs.
          APPEND zcl_wasm_f64_abs=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_ceil.
          APPEND zcl_wasm_f64_ceil=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_floor.
          APPEND zcl_wasm_f64_floor=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_trunc.
          APPEND zcl_wasm_f64_trunc=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_nearest.
          APPEND zcl_wasm_f64_nearest=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_sqrt.
          APPEND zcl_wasm_f64_sqrt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_neg.
          APPEND zcl_wasm_f32_neg=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_abs.
          APPEND zcl_wasm_f32_abs=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_store.
          APPEND zcl_wasm_f64_store=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_store8.
          APPEND zcl_wasm_i32_store8=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_copysign.
          APPEND zcl_wasm_f32_copysign=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_lt.
          APPEND zcl_wasm_f32_lt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_ge.
          APPEND zcl_wasm_f32_ge=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_store.
          APPEND zcl_wasm_i64_store=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_neg.
          APPEND zcl_wasm_f64_neg=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-br.
          APPEND zcl_wasm_br=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_gt.
          APPEND zcl_wasm_f32_gt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_const.
          APPEND zcl_wasm_f64_const=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_mul.
          APPEND zcl_wasm_f32_mul=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_min.
          APPEND zcl_wasm_f32_min=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_max.
          APPEND zcl_wasm_f32_max=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_div.
          APPEND zcl_wasm_f32_div=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_const.
          APPEND zcl_wasm_i32_const=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_const.
          APPEND zcl_wasm_i64_const=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-select.
          APPEND zcl_wasm_select=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_clz.
          APPEND zcl_wasm_i32_clz=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_ctz.
          APPEND zcl_wasm_i32_ctz=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_popcnt.
          APPEND zcl_wasm_i32_popcnt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_mul.
          APPEND zcl_wasm_i32_mul=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_div_s.
          APPEND zcl_wasm_i32_div_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_div_u.
          APPEND zcl_wasm_i32_div_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_div_s.
          APPEND zcl_wasm_i64_div_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_div_u.
          APPEND zcl_wasm_i64_div_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_rem_s.
          APPEND zcl_wasm_i64_rem_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_rem_u.
          APPEND zcl_wasm_i64_rem_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_and.
          APPEND zcl_wasm_i64_and=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_or.
          APPEND zcl_wasm_i64_or=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_xor.
          APPEND zcl_wasm_i64_xor=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_shl.
          APPEND zcl_wasm_i64_shl=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_shr_s.
          APPEND zcl_wasm_i64_shr_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_shr_u.
          APPEND zcl_wasm_i64_shr_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_rotl.
          APPEND zcl_wasm_i64_rotl=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_rotr.
          APPEND zcl_wasm_i64_rotr=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_rem_s.
          APPEND zcl_wasm_i32_rem_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_rem_u.
          APPEND zcl_wasm_i32_rem_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_and.
          APPEND zcl_wasm_i32_and=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_or.
          APPEND zcl_wasm_i32_or=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_xor.
          APPEND zcl_wasm_i32_xor=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_shl.
          APPEND zcl_wasm_i32_shl=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_shr_s.
          APPEND zcl_wasm_i32_shr_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_shr_u.
          APPEND zcl_wasm_i32_shr_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_rotl.
          APPEND zcl_wasm_i32_rotl=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_rotr.
          APPEND zcl_wasm_i32_rotr=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_extend8_s.
          APPEND zcl_wasm_i32_extend8_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_extend16_s.
          APPEND zcl_wasm_i32_extend16_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-call.
          APPEND zcl_wasm_call=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-if_.
          APPEND zcl_wasm_if=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-return_.
          APPEND zcl_wasm_return=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-unreachable.
          APPEND zcl_wasm_unreachable=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_load.
          APPEND zcl_wasm_i32_load=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_f32_s.
          APPEND zcl_wasm_i32_trunc_f32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_f32_u.
          APPEND zcl_wasm_i32_trunc_f32_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_f64_s.
          APPEND zcl_wasm_i32_trunc_f64_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_f64_u.
          APPEND zcl_wasm_i32_trunc_f64_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_extend_i32_s.
          APPEND zcl_wasm_i64_extend_i32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_reinterpret_f32.
          APPEND zcl_wasm_i32_reinterpret_f32=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_reinterpret_f64.
          APPEND zcl_wasm_i64_reinterpret_f64=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_reinterpret_i32.
          APPEND zcl_wasm_f32_reinterpret_i32=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_reinterpret_i64.
          APPEND zcl_wasm_f64_reinterpret_i64=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_extend_i32_u.
          APPEND zcl_wasm_i64_extend_i32_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_f32_s.
          APPEND zcl_wasm_i64_trunc_f32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_f32_u.
          APPEND zcl_wasm_i64_trunc_f32_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_f64_s.
          APPEND zcl_wasm_i64_trunc_f64_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_f64_u.
          APPEND zcl_wasm_i64_trunc_f64_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_convert_i32_s.
          APPEND zcl_wasm_f32_convert_i32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_convert_i32_u.
          APPEND zcl_wasm_f32_convert_i32_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_convert_i64_s.
          APPEND zcl_wasm_f32_convert_i64_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_convert_i64_u.
          APPEND zcl_wasm_f32_convert_i64_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_demote_f64.
          APPEND zcl_wasm_f32_demote_f64=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_convert_i32_s.
          APPEND zcl_wasm_f64_convert_i32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_convert_i32_u.
          APPEND zcl_wasm_f64_convert_i32_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_convert_i64_s.
          APPEND zcl_wasm_f64_convert_i64_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_convert_i64_u.
          APPEND zcl_wasm_f64_convert_i64_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_promote_f32.
          APPEND zcl_wasm_f64_promote_f32=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load.
          APPEND zcl_wasm_i64_load=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_load.
          APPEND zcl_wasm_f32_load=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_load.
          APPEND zcl_wasm_f64_load=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_load8_s.
          APPEND zcl_wasm_i32_load8_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_load8_u.
          APPEND zcl_wasm_i32_load8_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_add.
          APPEND zcl_wasm_f64_add=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_sub.
          APPEND zcl_wasm_f64_sub=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_mul.
          APPEND zcl_wasm_f64_mul=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_div.
          APPEND zcl_wasm_f64_div=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_min.
          APPEND zcl_wasm_f64_min=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_max.
          APPEND zcl_wasm_f64_max=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f64_copysign.
          APPEND zcl_wasm_f64_copysign=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_load16_s.
          APPEND zcl_wasm_i32_load16_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_load16_u.
          APPEND zcl_wasm_i32_load16_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_add.
          APPEND zcl_wasm_f32_add=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_clz.
          APPEND zcl_wasm_i64_clz=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_popcnt.
          APPEND zcl_wasm_i64_popcnt=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-select_star.
          APPEND zcl_wasm_select_star=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_extend8_s.
          APPEND zcl_wasm_i64_extend8_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_extend16_s.
          APPEND zcl_wasm_i64_extend16_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_extend32_s.
          APPEND zcl_wasm_i64_extend32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_add.
          APPEND zcl_wasm_i64_add=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load8_s.
          APPEND zcl_wasm_i64_load8_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load8_u.
          APPEND zcl_wasm_i64_load8_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_eqz.
          APPEND zcl_wasm_i64_eqz=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load16_s.
          APPEND zcl_wasm_i64_load16_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-table_get.
          APPEND zcl_wasm_table_get=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-table_set.
          APPEND zcl_wasm_table_set=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-f32_store.
          APPEND zcl_wasm_f32_store=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i32_store16.
          APPEND zcl_wasm_i32_store16=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_store8.
          APPEND zcl_wasm_i64_store8=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_store32.
          APPEND zcl_wasm_i64_store32=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load16_u.
          APPEND zcl_wasm_i64_load16_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-ref_null.
          APPEND zcl_wasm_ref_null=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-ref_is_null.
          APPEND zcl_wasm_ref_is_null=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-ref_func.
          APPEND zcl_wasm_ref_func=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load32_s.
          APPEND zcl_wasm_i64_load32_s=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-i64_load32_u.
          APPEND zcl_wasm_i64_load32_u=>parse( io_body ) TO et_instructions.
        WHEN zif_wasm_opcodes=>c_opcodes-drop.
          APPEND zcl_wasm_drop=>parse( io_body ) TO et_instructions.
        WHEN 'FC'.
          DATA(lv_opcodei) = io_body->shift_u32( ).
          CASE lv_opcodei.
            WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_sat_f32_s.
              APPEND zcl_wasm_i32_trunc_sat_f32_s=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_sat_f32_u.
              APPEND zcl_wasm_i32_trunc_sat_f32_u=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_sat_f64_s.
              APPEND zcl_wasm_i32_trunc_sat_f64_s=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i32_trunc_sat_f64_u.
              APPEND zcl_wasm_i32_trunc_sat_f64_u=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_sat_f32_s.
              APPEND zcl_wasm_i64_trunc_sat_f32_s=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_sat_f32_u.
              APPEND zcl_wasm_i64_trunc_sat_f32_u=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_sat_f64_s.
              APPEND zcl_wasm_i64_trunc_sat_f64_s=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-i64_trunc_sat_f64_u.
              APPEND zcl_wasm_i64_trunc_sat_f64_u=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-memory_init.
              APPEND zcl_wasm_memory_init=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-data_drop.
              APPEND zcl_wasm_data_drop=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-memory_copy.
              APPEND zcl_wasm_memory_copy=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-memory_fill.
              APPEND zcl_wasm_memory_fill=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-table_init.
              APPEND zcl_wasm_table_init=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-elem_drop.
              APPEND zcl_wasm_elem_drop=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-table_copy.
              APPEND zcl_wasm_table_copy=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-table_grow.
              APPEND zcl_wasm_table_grow=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-table_size.
              APPEND zcl_wasm_table_size=>parse( io_body ) TO et_instructions.
            WHEN zif_wasm_opcodes=>c_opcodes-table_fill.
              APPEND zcl_wasm_table_fill=>parse( io_body ) TO et_instructions.
            WHEN OTHERS.
              RAISE EXCEPTION NEW zcx_wasm( text = |todoparser FC: { lv_opcodei }| ).
          ENDCASE.
        WHEN zif_wasm_opcodes=>c_opcodes-end.
          APPEND zcl_wasm_end=>parse( io_body ) TO et_instructions.
          RETURN.
        WHEN zif_wasm_opcodes=>c_opcodes-else_.
          RETURN.
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |todoparser: { lv_opcode }| ).
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_function.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec

    DO io_body->shift_u32( ) TIMES.
      APPEND io_body->shift_u32( ) TO rt_results.
    ENDDO.

  ENDMETHOD.


  METHOD parse_data.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-datasec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_type) = io_body->shift_u32( ).

      CASE lv_type.
        WHEN 0.
          parse_instructions(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = DATA(lv_last_opcode)
              et_instructions = DATA(lt_instructions) ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          DATA(lv_vec) = io_body->shift_u32( ).
          DATA(lv_contents) = io_body->shift( lv_vec ).
        WHEN 1.
          lv_vec = io_body->shift_u32( ).
          lv_contents = io_body->shift( lv_vec ).
        WHEN 2.
          DATA(lv_memidx) = io_body->shift_u32( ).

          parse_instructions(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          ASSERT lv_last_opcode = zif_wasm_opcodes=>c_opcodes-end.

          lv_vec = io_body->shift_u32( ).
          lv_contents = io_body->shift( lv_vec ).
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |parse_data, type: { lv_type }| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

  METHOD parse_table.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-tablesec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_reftype) = io_body->shift( 1 ).

      CASE lv_reftype.
        WHEN '70' OR '6F'.
          DATA(lv_limit) = io_body->shift( 1 ).

          CASE lv_limit.
            WHEN '00'.
              DATA(lv_min) = io_body->shift_u32( ).
              DATA(lv_max) = 0.
            WHEN '01'.
              lv_min = io_body->shift_u32( ).
              lv_max = io_body->shift_u32( ).
            WHEN OTHERS.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_table: todo| ).
          ENDCASE.
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |parse_table: todo| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

  METHOD parse_global.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-globalsec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_type) = io_body->shift( 1 ).
      DATA(lv_mut) = io_body->shift( 1 ).

      " WRITE: / 'type:', lv_type.
      " WRITE: / 'mut:', lv_mut.

      parse_instructions(
        EXPORTING
          io_body         = io_body
        IMPORTING
          ev_last_opcode  = DATA(lv_last_opcode)
          et_instructions = DATA(lt_instructions) ).
      IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
        RAISE EXCEPTION NEW zcx_wasm( text = |parse_global, expected end| ).
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD parse_element.

* https://webassembly.github.io/spec/core/binary/modules.html#binary-elemsec

    DO io_body->shift_u32( ) TIMES.
      DATA(lv_type) = io_body->shift_u32( ).

      CASE lv_type.
        WHEN 0.
          parse_instructions(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = DATA(lv_last_opcode)
              et_instructions = DATA(lt_instructions) ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          DO io_body->shift_u32( ) TIMES.
            DATA(lv_funcidx) = io_body->shift_u32( ).
            " WRITE: / 'funcidx', lv_funcidx.
          ENDDO.
        WHEN 1.
          DATA(lv_elemkind) = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            lv_funcidx = io_body->shift_u32( ).
            " WRITE: / 'funcidx', lv_funcidx.
          ENDDO.
        WHEN 2.
          DATA(lv_tableidx) = io_body->shift_u32( ).

          parse_instructions(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          lv_elemkind = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            lv_funcidx = io_body->shift_u32( ).
            " WRITE: / 'funcidx', lv_funcidx.
          ENDDO.
        WHEN 3.
          lv_elemkind = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            lv_funcidx = io_body->shift_u32( ).
          ENDDO.
        WHEN 4.
          parse_instructions(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          DO io_body->shift_u32( ) TIMES.
            parse_instructions(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN 5.
          DATA(lv_reftype) = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            parse_instructions(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN 6.
          lv_tableidx = io_body->shift_u32( ).

          parse_instructions(
            EXPORTING
              io_body         = io_body
            IMPORTING
              ev_last_opcode  = lv_last_opcode
              et_instructions = lt_instructions ).
          IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
            RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
          ENDIF.

          lv_reftype = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            parse_instructions(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN 7.
          lv_reftype = io_body->shift( 1 ).

          DO io_body->shift_u32( ) TIMES.
            parse_instructions(
              EXPORTING
                io_body         = io_body
              IMPORTING
                ev_last_opcode  = lv_last_opcode
                et_instructions = lt_instructions ).
            IF lv_last_opcode <> zif_wasm_opcodes=>c_opcodes-end.
              RAISE EXCEPTION NEW zcx_wasm( text = |parse_element: expected end| ).
            ENDIF.
          ENDDO.
        WHEN OTHERS.
          RAISE EXCEPTION NEW zcx_wasm( text = |elementtype: { lv_type }| ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
