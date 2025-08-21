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
    DATA call       TYPE zmcp_call.
    DATA class_code TYPE string.
    DATA param      TYPE zmcp_param.

    cut = NEW #( ).
    class_code = 'CLASS ZTEST DEFINITION    .    PUBLIC SECTION.   methods : ' && cl_abap_char_utilities=>newline &&
                 'test importing field1 type string   returning value(field2) type string.    PROTECTED SECTION.   ' && cl_abap_char_utilities=>newline &&
                 'PRIVATE SECTION. ENDCLASS.  CLASS ZTEST IMPLEMENTATION.   METHOD test.      field2 = field1 && field1.   ENDMETHOD.  ENDCLASS.'.

    call-method_name = 'TEST'.
    param-name      = 'FIELD1'.
    param-direction = 'IMPORTING'.
    param-value     = 'FEFE'.
    APPEND param TO call-params.

    param-name      = 'FIELD2'.
    param-direction = 'RETURNING'.
    param-value     = ''.
    APPEND param TO call-params.

    cut->run( EXPORTING class_name  = 'ZTEST'
                        class_code  = class_code
                        input_call  = call
              IMPORTING result      = DATA(result)
                        message     = DATA(message) ).
    
    " Check that method was executed successfully
    cl_abap_unit_assert=>assert_initial( message-message ).
    
    " Find the FIELD2 parameter in the result
    READ TABLE result-params INTO DATA(result_param) WITH KEY name = 'FIELD2'.
    cl_abap_unit_assert=>assert_subrc( exp = 0 ).
    cl_abap_unit_assert=>assert_equals( exp = 'FEFEFEFE'
                                        act = result_param-value ).
  ENDMETHOD.

  METHOD setup.
  ENDMETHOD.
ENDCLASS.