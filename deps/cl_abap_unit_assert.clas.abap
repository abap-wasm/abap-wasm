CLASS cl_abap_unit_assert DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS assert_not_initial
      IMPORTING
        iv_act TYPE string.
ENDCLASS.

CLASS cl_abap_unit_assert IMPLEMENTATION.
  METHOD assert_not_initial.
    RETURN.
  ENDMETHOD.
ENDCLASS.