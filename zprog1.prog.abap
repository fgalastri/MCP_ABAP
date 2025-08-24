*&---------------------------------------------------------------------*
*& Report zprog1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprog1.



CLASS ztt1 DEFINITION    .
  PUBLIC SECTION.
    METHODS test IMPORTING field1        TYPE string
                 RETURNING VALUE(field2) TYPE string.
ENDCLASS.


CLASS ztt1 IMPLEMENTATION.
  METHOD test.
    field2 = field1 && field1.
  ENDMETHOD.
ENDCLASS.
