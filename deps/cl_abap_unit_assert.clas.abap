CLASS cl_abap_unit_assert DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS
      assert_equals
        IMPORTING
          act TYPE string
          exp TYPE string
          msg TYPE string OPTIONAL.

    CLASS-METHODS
      assert_differs
        IMPORTING
          act TYPE string
          exp TYPE string.

    CLASS-METHODS
      assert_not_initial
        IMPORTING
          act TYPE any.

    CLASS-METHODS
      assert_initial
        IMPORTING
          act TYPE any.
ENDCLASS.

CLASS cl_abap_unit_assert IMPLEMENTATION.
  METHOD assert_not_initial.
    RETURN.
  ENDMETHOD.

  METHOD assert_equals.
    RETURN.
  ENDMETHOD.

  METHOD assert_differs.
    RETURN.
  ENDMETHOD.

  METHOD assert_initial.
    RETURN.
  ENDMETHOD.
ENDCLASS.