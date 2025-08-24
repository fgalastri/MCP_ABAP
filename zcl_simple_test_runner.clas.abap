CLASS zcl_simple_test_runner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_simple_test.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_simple_test_runner IMPLEMENTATION.

  METHOD run_simple_test.
    DATA: lo_validator TYPE REF TO zcl_hybrid_validator,
          ls_result TYPE zcl_hybrid_validator=>ty_validation_result,
          ls_message TYPE zcl_hybrid_validator=>ty_message,
          ls_call TYPE zcl_hybrid_validator=>ty_call,
          lt_params TYPE zcl_hybrid_validator=>ty_param_tab.

    WRITE: / 'Testing Hybrid Validator',
           / '========================'.

    " Create simple test class
    DATA(lv_test_class) = |CLASS zcl_simple_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS test_simple_method
      IMPORTING iv_input TYPE string
      EXPORTING ev_output TYPE string
                ev_length TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_simple_test IMPLEMENTATION.
  METHOD test_simple_method.
    ev_output = |Hello { iv_input }!|.
    ev_length = strlen( ev_output ).
  ENDMETHOD.
ENDCLASS.|.

    " Test 1: Syntax validation only
    WRITE: / 'Test 1: Syntax Validation Only'.
    
    CREATE OBJECT lo_validator.
    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_SIMPLE_TEST'
               class_code = lv_test_class
      IMPORTING result = ls_result
               message = ls_message
    ).

    IF ls_result-success = abap_true.
      WRITE: / '  ✓ Syntax validation successful'.
      WRITE: / |  ✓ Time: { ls_result-execution_time }ms|.
    ELSE.
      WRITE: / |  ✗ Syntax validation failed: { ls_result-message }|.
    ENDIF.

    " Test 2: Method execution with simple parameters
    WRITE: / 'Test 2: Method Execution with Simple Parameters'.

    " Create parameters
    APPEND VALUE zcl_hybrid_validator=>ty_param(
      name = 'IV_INPUT'
      direction = 'IMPORTING'
      type = 'STRING'
      value = 'World'
    ) TO lt_params.

    APPEND VALUE zcl_hybrid_validator=>ty_param(
      name = 'EV_OUTPUT'
      direction = 'EXPORTING'
      type = 'STRING'
      value = ''
    ) TO lt_params.

    APPEND VALUE zcl_hybrid_validator=>ty_param(
      name = 'EV_LENGTH'
      direction = 'EXPORTING'
      type = 'I'
      value = ''
    ) TO lt_params.

    ls_call-method_name = 'TEST_SIMPLE_METHOD'.
    ls_call-params = lt_params.

    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_SIMPLE_TEST'
               class_code = lv_test_class
               input_call = ls_call
      IMPORTING result = ls_result
               message = ls_message
    ).

    IF ls_result-success = abap_true.
      WRITE: / '  ✓ Method execution successful'.
      WRITE: / |  ✓ Used XCO execution: { ls_result-used_xco_execution }|.
      WRITE: / |  ✓ Temp class: { ls_result-temp_class_name }|.
      WRITE: / |  ✓ Time: { ls_result-execution_time }ms|.
      
      " Show results
      LOOP AT ls_result-result-params INTO DATA(ls_param).
        WRITE: / |  ✓ { ls_param-name } = { ls_param-value }|.
      ENDLOOP.
    ELSE.
      WRITE: / |  ✗ Method execution failed: { ls_result-message }|.
    ENDIF.

    WRITE: / '========================',
           / 'Test completed!'.
  ENDMETHOD.

ENDCLASS.
