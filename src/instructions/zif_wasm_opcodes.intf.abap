INTERFACE zif_wasm_opcodes PUBLIC.

  TYPES ty_opcode TYPE x LENGTH 1.
  TYPES ty_opcodei TYPE int8.

  CONSTANTS:
      BEGIN OF c_opcodes,
* https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
        unreachable   TYPE ty_opcode VALUE '00',
        nop           TYPE ty_opcode VALUE '01',
        block         TYPE ty_opcode VALUE '02',
        loop          TYPE ty_opcode VALUE '03',
        if_           TYPE ty_opcode VALUE '04',
        else_         TYPE ty_opcode VALUE '05',
        end           TYPE ty_opcode VALUE '0B',
        br            TYPE ty_opcode VALUE '0C',
        br_if         TYPE ty_opcode VALUE '0D',
        br_table      TYPE ty_opcode VALUE '0E',
        return_       TYPE ty_opcode VALUE '0F',
        call          TYPE ty_opcode VALUE '10',
        call_indirect TYPE ty_opcode VALUE '11',
* https://webassembly.github.io/spec/core/binary/instructions.html#parametric-instructions
        drop          TYPE ty_opcode VALUE '1A',
        select        TYPE ty_opcode VALUE '1B',
        select_star   TYPE ty_opcode VALUE '1C',
* https://webassembly.github.io/spec/core/binary/instructions.html#variable-instructions
        local_get     TYPE ty_opcode VALUE '20',
        local_set     TYPE ty_opcode VALUE '21',
        local_tee     TYPE ty_opcode VALUE '22',
        global_get    TYPE ty_opcode VALUE '23',
        global_set    TYPE ty_opcode VALUE '24',
        table_get     TYPE ty_opcode VALUE '25',
        table_set     TYPE ty_opcode VALUE '26',
        i32_load       TYPE ty_opcode VALUE '28',
        i64_load       TYPE ty_opcode VALUE '29',
        f32_load       TYPE ty_opcode VALUE '2A',
        f64_load       TYPE ty_opcode VALUE '2B',
        i32_load8_s    TYPE ty_opcode VALUE '2C',
        i32_load8_u    TYPE ty_opcode VALUE '2D',
        i32_load16_s   TYPE ty_opcode VALUE '2E',
        i32_load16_u   TYPE ty_opcode VALUE '2F',
        i64_load8_s    TYPE ty_opcode VALUE '30',
        i64_load8_u    TYPE ty_opcode VALUE '31',
        i64_load16_s   TYPE ty_opcode VALUE '32',
        i64_load16_u   TYPE ty_opcode VALUE '33',
        i64_load32_s   TYPE ty_opcode VALUE '34',
        i64_load32_u   TYPE ty_opcode VALUE '35',
        i32_store      TYPE ty_opcode VALUE '36',
        i64_store      TYPE ty_opcode VALUE '37',
        f32_store      TYPE ty_opcode VALUE '38',
        f64_store      TYPE ty_opcode VALUE '39',
        i32_store8     TYPE ty_opcode VALUE '3A',
        i32_store16    TYPE ty_opcode VALUE '3B',
        i64_store8     TYPE ty_opcode VALUE '3C',
        i64_store16    TYPE ty_opcode VALUE '3D',
        i64_store32    TYPE ty_opcode VALUE '3E',
        memory_size    TYPE ty_opcode VALUE '3F',
        memory_grow    TYPE ty_opcode VALUE '40',
* https://webassembly.github.io/spec/core/binary/instructions.html#numeric-instructions
        i32_const      TYPE ty_opcode VALUE '41',
        i64_const      TYPE ty_opcode VALUE '42',
        f32_const      TYPE ty_opcode VALUE '43',
        f64_const      TYPE ty_opcode VALUE '44',
        i32_eqz        TYPE ty_opcode VALUE '45',
        i32_eq         TYPE ty_opcode VALUE '46',
        i32_ne         TYPE ty_opcode VALUE '47',
        i32_lt_s       TYPE ty_opcode VALUE '48',
        i32_lt_u       TYPE ty_opcode VALUE '49',
        i32_gt_s       TYPE ty_opcode VALUE '4A',
        i32_gt_u       TYPE ty_opcode VALUE '4B',
        i32_le_s       TYPE ty_opcode VALUE '4C',
        i32_le_u       TYPE ty_opcode VALUE '4D',
        i32_ge_s       TYPE ty_opcode VALUE '4E',
        i32_ge_u       TYPE ty_opcode VALUE '4F',
        i64_eqz        TYPE ty_opcode VALUE '50',
        i64_eq         TYPE ty_opcode VALUE '51',
        i64_ne         TYPE ty_opcode VALUE '52',
        i64_lt_s       TYPE ty_opcode VALUE '53',
        i64_lt_u       TYPE ty_opcode VALUE '54',
        i64_gt_s       TYPE ty_opcode VALUE '55',
        i64_gt_u       TYPE ty_opcode VALUE '56',
        i64_le_s       TYPE ty_opcode VALUE '57',
        i64_le_u       TYPE ty_opcode VALUE '58',
        i64_ge_s       TYPE ty_opcode VALUE '59',
        i64_ge_u       TYPE ty_opcode VALUE '5A',
        f32_eq         TYPE ty_opcode VALUE '5B',
        f32_ne         TYPE ty_opcode VALUE '5C',
        f32_lt         TYPE ty_opcode VALUE '5D',
        f32_gt         TYPE ty_opcode VALUE '5E',
        f32_le         TYPE ty_opcode VALUE '5F',
        f32_ge         TYPE ty_opcode VALUE '60',
        f64_eq         TYPE ty_opcode VALUE '61',
        f64_ne         TYPE ty_opcode VALUE '62',
        f64_lt         TYPE ty_opcode VALUE '63',
        f64_gt         TYPE ty_opcode VALUE '64',
        f64_le         TYPE ty_opcode VALUE '65',
        f64_ge         TYPE ty_opcode VALUE '66',
        i32_clz        TYPE ty_opcode VALUE '67',
        i32_ctz        TYPE ty_opcode VALUE '68',
        i32_popcnt     TYPE ty_opcode VALUE '69',
        i32_add        TYPE ty_opcode VALUE '6A',
        i32_sub        TYPE ty_opcode VALUE '6B',
        i32_mul        TYPE ty_opcode VALUE '6C',
        i32_div_s      TYPE ty_opcode VALUE '6D',
        i32_div_u      TYPE ty_opcode VALUE '6E',
        i32_rem_s      TYPE ty_opcode VALUE '6F',
        i32_rem_u      TYPE ty_opcode VALUE '70',
        i32_and        TYPE ty_opcode VALUE '71',
        i32_or         TYPE ty_opcode VALUE '72',
        i32_xor        TYPE ty_opcode VALUE '73',
        i32_shl        TYPE ty_opcode VALUE '74',
        i32_shr_s      TYPE ty_opcode VALUE '75',
        i32_shr_u      TYPE ty_opcode VALUE '76',
        i32_rotl       TYPE ty_opcode VALUE '77',
        i32_rotr       TYPE ty_opcode VALUE '78',
        i64_clz        TYPE ty_opcode VALUE '79',
        i64_ctz        TYPE ty_opcode VALUE '7A',
        i64_popcnt     TYPE ty_opcode VALUE '7B',
        i64_add        TYPE ty_opcode VALUE '7C',
        i64_sub        TYPE ty_opcode VALUE '7D',
        i64_mul        TYPE ty_opcode VALUE '7E',
        i64_div_s      TYPE ty_opcode VALUE '7F',
        i64_div_u      TYPE ty_opcode VALUE '80',
        i64_rem_s      TYPE ty_opcode VALUE '81',
        i64_rem_u      TYPE ty_opcode VALUE '82',
        i64_and        TYPE ty_opcode VALUE '83',
        i64_or         TYPE ty_opcode VALUE '84',
        i64_xor        TYPE ty_opcode VALUE '85',
        i64_shl        TYPE ty_opcode VALUE '86',
        i64_shr_s      TYPE ty_opcode VALUE '87',
        i64_shr_u      TYPE ty_opcode VALUE '88',
        i64_rotl       TYPE ty_opcode VALUE '89',
        i64_rotr       TYPE ty_opcode VALUE '8A',
        f32_abs        TYPE ty_opcode VALUE '8B',
        f32_neg        TYPE ty_opcode VALUE '8C',
        f32_ceil       TYPE ty_opcode VALUE '8D',
        f32_floor      TYPE ty_opcode VALUE '8E',
        f32_trunc      TYPE ty_opcode VALUE '8F',
        f32_nearest    TYPE ty_opcode VALUE '90',
        f32_sqrt       TYPE ty_opcode VALUE '91',
        f32_add        TYPE ty_opcode VALUE '92',
        f32_sub        TYPE ty_opcode VALUE '93',
        f32_mul        TYPE ty_opcode VALUE '94',
        f32_div        TYPE ty_opcode VALUE '95',
        f32_min        TYPE ty_opcode VALUE '96',
        f32_max        TYPE ty_opcode VALUE '97',
        f32_copysign   TYPE ty_opcode VALUE '98',
        f64_abs        TYPE ty_opcode VALUE '99',
        f64_neg        TYPE ty_opcode VALUE '9A',
        f64_ceil       TYPE ty_opcode VALUE '9B',
        f64_floor      TYPE ty_opcode VALUE '9C',
        f64_trunc      TYPE ty_opcode VALUE '9D',
        f64_nearest    TYPE ty_opcode VALUE '9E',
        f64_sqrt       TYPE ty_opcode VALUE '9F',
        f64_add        TYPE ty_opcode VALUE 'A0',
        f64_sub        TYPE ty_opcode VALUE 'A1',
        f64_mul        TYPE ty_opcode VALUE 'A2',
        f64_div        TYPE ty_opcode VALUE 'A3',
        f64_min        TYPE ty_opcode VALUE 'A4',
        f64_max        TYPE ty_opcode VALUE 'A5',
        f64_copysign   TYPE ty_opcode VALUE 'A6',
        i32_wrap_i64        TYPE ty_opcode VALUE 'A7',
        i32_trunc_f32_s     TYPE ty_opcode VALUE 'A8',
        i32_trunc_f32_u     TYPE ty_opcode VALUE 'A9',
        i32_trunc_f64_s     TYPE ty_opcode VALUE 'AA',
        i32_trunc_f64_u     TYPE ty_opcode VALUE 'AB',
        i64_extend_i32_s    TYPE ty_opcode VALUE 'AC',
        i64_extend_i32_u    TYPE ty_opcode VALUE 'AD',
        i64_trunc_f32_s     TYPE ty_opcode VALUE 'AE',
        i64_trunc_f32_u     TYPE ty_opcode VALUE 'AF',
        i64_trunc_f64_s     TYPE ty_opcode VALUE 'B0',
        i64_trunc_f64_u     TYPE ty_opcode VALUE 'B1',
        f32_convert_i32_s   TYPE ty_opcode VALUE 'B2',
        f32_convert_i32_u   TYPE ty_opcode VALUE 'B3',
        f32_convert_i64_s   TYPE ty_opcode VALUE 'B4',
        f32_convert_i64_u   TYPE ty_opcode VALUE 'B5',
        f32_demote_f64      TYPE ty_opcode VALUE 'B6',
        f64_convert_i32_s   TYPE ty_opcode VALUE 'B7',
        f64_convert_i32_u   TYPE ty_opcode VALUE 'B8',
        f64_convert_i64_s   TYPE ty_opcode VALUE 'B9',
        f64_convert_i64_u   TYPE ty_opcode VALUE 'BA',
        f64_promote_f32     TYPE ty_opcode VALUE 'BB',
        i32_reinterpret_f32 TYPE ty_opcode VALUE 'BC',
        i64_reinterpret_f64 TYPE ty_opcode VALUE 'BD',
        f32_reinterpret_i32 TYPE ty_opcode VALUE 'BE',
        f64_reinterpret_i64 TYPE ty_opcode VALUE 'BF',
        i32_extend8_s       TYPE ty_opcode VALUE 'C0',
        i32_extend16_s      TYPE ty_opcode VALUE 'C1',
        i64_extend8_s       TYPE ty_opcode VALUE 'C2',
        i64_extend16_s      TYPE ty_opcode VALUE 'C3',
        i64_extend32_s      TYPE ty_opcode VALUE 'C4',
        ref_null            TYPE ty_opcode VALUE 'D0',
        ref_is_null         TYPE ty_opcode VALUE 'D1',
        ref_func            TYPE ty_opcode VALUE 'D2',
* FD prefixed = SIMD
* FC opcodes:
        i32_trunc_sat_f32_s TYPE ty_opcodei VALUE 0,
        i32_trunc_sat_f32_u TYPE ty_opcodei VALUE 1,
        i32_trunc_sat_f64_s TYPE ty_opcodei VALUE 2,
        i32_trunc_sat_f64_u TYPE ty_opcodei VALUE 3,
        i64_trunc_sat_f32_s TYPE ty_opcodei VALUE 4,
        i64_trunc_sat_f32_u TYPE ty_opcodei VALUE 5,
        i64_trunc_sat_f64_s TYPE ty_opcodei VALUE 6,
        i64_trunc_sat_f64_u TYPE ty_opcodei VALUE 7,
        memory_init         TYPE ty_opcodei VALUE 8,
        data_drop           TYPE ty_opcodei VALUE 9,
        memory_copy         TYPE ty_opcodei VALUE 10,
        memory_fill         TYPE ty_opcodei VALUE 11,
        table_init          TYPE ty_opcodei VALUE 12,
        elem_drop           TYPE ty_opcodei VALUE 13,
        table_copy          TYPE ty_opcodei VALUE 14,
        table_grow          TYPE ty_opcodei VALUE 15,
        table_size          TYPE ty_opcodei VALUE 16,
        table_fill          TYPE ty_opcodei VALUE 17,
      END OF c_opcodes.

ENDINTERFACE.
