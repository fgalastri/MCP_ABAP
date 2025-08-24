CLASS zmcp_method_validate_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    DATA cut TYPE REF TO zmcp_method_validate.

  PRIVATE SECTION.
    " Test methods
    METHODS test          FOR TESTING.
    METHODS setup.

ENDCLASS.

CLASS zmcp_method_validate_test IMPLEMENTATION.
  METHOD test.
    DATA calls      TYPE zmcp_method_validate=>ty_call_tab.
    DATA call       LIKE LINE OF calls.
    DATA class_code TYPE string.
    DATA param      TYPE zmcp_method_validate=>ty_param.

    cut = NEW #( ).
    class_code = 'CLASS ZTEST DEFINITION    .    PUBLIC SECTION.   methods : ' && cl_abap_char_utilities=>newline &&
                 'test importing field1 type string   returning value(field2) type string.    PROTECTED SECTION.   ' && cl_abap_char_utilities=>newline &&
                 'PRIVATE SECTION. ENDCLASS.  CLASS ZTEST IMPLEMENTATION.   METHOD test.      field2 = field1 && field1.   ENDMETHOD.  ENDCLASS.'.

    call-method_name = 'TEST'.
    param-name      = 'FIELD1'.
    param-direction = 'EXPORTING'.
    param-value     = 'FEFE'.
    APPEND param TO call-params.

    param-name      = 'FIELD2'.
    param-direction = 'RECEIVING'.
    param-value     = ''.
    APPEND param TO call-params.

    cut->run( EXPORTING class_name = 'ZTEST'
                        class_code = class_code
                        input_call = call
              IMPORTING result     = DATA(result) ).
    cl_abap_unit_assert=>assert_equals( exp = 'FEFEFEFE'
                                        act = result-params[ 1 ]-value ).
  ENDMETHOD.

  METHOD setup.
  ENDMETHOD.
ENDCLASS.
