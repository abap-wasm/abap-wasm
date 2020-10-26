
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS: add FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD add.

    DATA(lo_memory) = NEW zcl_wasm_memory( ).

    zcl_wasm_i32=>add( lo_memory ).



  ENDMETHOD.

ENDCLASS.
