CLASS zcl_test_hybrid_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Test runner for the hybrid validator
    METHODS run_all_tests.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Test methods for different scenarios
    METHODS test_simple_string_parameters.
    METHODS test_dictionary_types.
    METHODS test_local_types.
    METHODS test_syntax_validation_only.
    METHODS test_complex_json_parameters.

    " Helper methods
    METHODS create_test_call
      IMPORTING iv_method_name TYPE string
                it_params TYPE zcl_hybrid_method_validator=>ty_param_tab
      RETURNING VALUE(rs_call) TYPE zcl_hybrid_method_validator=>ty_call.

    METHODS create_param
      IMPORTING iv_name TYPE string
                iv_direction TYPE string
                iv_type TYPE string
                iv_value TYPE string
      RETURNING VALUE(rs_param) TYPE zcl_hybrid_method_validator=>ty_param.

    METHODS log_test_result
      IMPORTING iv_test_name TYPE string
                is_result TYPE zcl_hybrid_method_validator=>ty_validation_result.
ENDCLASS.

CLASS zcl_test_hybrid_validator IMPLEMENTATION.

  METHOD run_all_tests.
    WRITE: / 'Starting Hybrid Method Validator Tests',
           / '=========================================='.

    " Test 1: Simple string parameters
    test_simple_string_parameters( ).

    " Test 2: Dictionary types (MARA)
    test_dictionary_types( ).

    " Test 3: Local types (the challenging scenario)
    test_local_types( ).

    " Test 4: Syntax validation only
    test_syntax_validation_only( ).

    " Test 5: Complex JSON parameters
    test_complex_json_parameters( ).

    WRITE: / '==========================================',
           / 'All tests completed!'.
  ENDMETHOD.

  METHOD test_simple_string_parameters.
    DATA: lo_validator TYPE REF TO zcl_hybrid_method_validator,
          ls_result TYPE zcl_hybrid_method_validator=>ty_validation_result,
          ls_message TYPE zcl_hybrid_method_validator=>ty_message,
          ls_call TYPE zcl_hybrid_method_validator=>ty_call,
          lt_params TYPE zcl_hybrid_method_validator=>ty_param_tab.

    WRITE: / 'Test 1: Simple String Parameters'.

    " Get test class source
    DATA(lv_class_source) = |CLASS zcl_test_validation_scenarios DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS test_simple_strings
      IMPORTING iv_input_text TYPE string
                iv_prefix TYPE string OPTIONAL
      EXPORTING ev_result TYPE string
                ev_length TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_test_validation_scenarios IMPLEMENTATION.
  METHOD test_simple_strings.
    DATA: lv_temp TYPE string.
    
    IF iv_prefix IS NOT INITIAL.
      lv_temp = |{ iv_prefix }: { iv_input_text }|.
    ELSE.
      lv_temp = iv_input_text.
    ENDIF.
    
    ev_result = lv_temp.
    ev_length = strlen( lv_temp ).
  ENDMETHOD.
ENDCLASS.|.

    " Create parameters
    APPEND create_param( iv_name = 'IV_INPUT_TEXT' iv_direction = 'IMPORTING' 
                        iv_type = 'STRING' iv_value = 'Hello World' ) TO lt_params.
    APPEND create_param( iv_name = 'IV_PREFIX' iv_direction = 'IMPORTING' 
                        iv_type = 'STRING' iv_value = 'Test' ) TO lt_params.
    APPEND create_param( iv_name = 'EV_RESULT' iv_direction = 'EXPORTING' 
                        iv_type = 'STRING' iv_value = '' ) TO lt_params.
    APPEND create_param( iv_name = 'EV_LENGTH' iv_direction = 'EXPORTING' 
                        iv_type = 'I' iv_value = '' ) TO lt_params.

    ls_call = create_test_call( iv_method_name = 'TEST_SIMPLE_STRINGS' it_params = lt_params ).

    " Test with hybrid validator
    CREATE OBJECT lo_validator.
    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_TEST_VALIDATION_SCENARIOS'
               class_code = lv_class_source
               input_call = ls_call
      IMPORTING result = ls_result
               message = ls_message
    ).

    log_test_result( iv_test_name = 'Simple String Parameters' is_result = ls_result ).
  ENDMETHOD.

  METHOD test_dictionary_types.
    DATA: lo_validator TYPE REF TO zcl_hybrid_method_validator,
          ls_result TYPE zcl_hybrid_method_validator=>ty_validation_result,
          ls_message TYPE zcl_hybrid_method_validator=>ty_message,
          ls_call TYPE zcl_hybrid_method_validator=>ty_call,
          lt_params TYPE zcl_hybrid_method_validator=>ty_param_tab.

    WRITE: / 'Test 2: Dictionary Types (MARA)'.

    " Get test class source with MARA
    DATA(lv_class_source) = |CLASS zcl_test_validation_scenarios DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS test_material_info
      IMPORTING is_material TYPE mara
      EXPORTING ev_material_type TYPE string
                ev_description TYPE string
                ev_valid TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_test_validation_scenarios IMPLEMENTATION.
  METHOD test_material_info.
    ev_valid = abap_false.
    
    IF is_material-matnr IS NOT INITIAL.
      ev_valid = abap_true.
      
      CASE is_material-mtart.
        WHEN 'FERT'. ev_material_type = 'Finished Product'.
        WHEN 'HALB'. ev_material_type = 'Semi-Finished Product'.
        WHEN 'ROH'.  ev_material_type = 'Raw Material'.
        WHEN OTHERS. ev_material_type = 'Other Material Type'.
      ENDCASE.
      
      ev_description = |Material { is_material-matnr } - Type: { ev_material_type }|.
    ELSE.
      ev_description = 'Invalid material - no material number provided'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.|.

    " Create MARA structure as JSON
    DATA(lv_mara_json) = |{"matnr":"TEST123","mtart":"FERT","mbrsh":"M","meins":"EA"}|.

    " Create parameters
    APPEND create_param( iv_name = 'IS_MATERIAL' iv_direction = 'IMPORTING' 
                        iv_type = 'MARA' iv_value = lv_mara_json ) TO lt_params.
    APPEND create_param( iv_name = 'EV_MATERIAL_TYPE' iv_direction = 'EXPORTING' 
                        iv_type = 'STRING' iv_value = '' ) TO lt_params.
    APPEND create_param( iv_name = 'EV_DESCRIPTION' iv_direction = 'EXPORTING' 
                        iv_type = 'STRING' iv_value = '' ) TO lt_params.
    APPEND create_param( iv_name = 'EV_VALID' iv_direction = 'EXPORTING' 
                        iv_type = 'ABAP_BOOL' iv_value = '' ) TO lt_params.

    ls_call = create_test_call( iv_method_name = 'TEST_MATERIAL_INFO' it_params = lt_params ).

    " Test with hybrid validator
    CREATE OBJECT lo_validator.
    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_TEST_VALIDATION_SCENARIOS'
               class_code = lv_class_source
               input_call = ls_call
      IMPORTING result = ls_result
               message = ls_message
    ).

    log_test_result( iv_test_name = 'Dictionary Types (MARA)' is_result = ls_result ).
  ENDMETHOD.

  METHOD test_local_types.
    DATA: lo_validator TYPE REF TO zcl_hybrid_method_validator,
          ls_result TYPE zcl_hybrid_method_validator=>ty_validation_result,
          ls_message TYPE zcl_hybrid_method_validator=>ty_message,
          ls_call TYPE zcl_hybrid_method_validator=>ty_call,
          lt_params TYPE zcl_hybrid_method_validator=>ty_param_tab.

    WRITE: / 'Test 3: Local Types (The Challenge!)'.

    " Get test class source with local types
    DATA(lv_class_source) = |CLASS zcl_test_validation_scenarios DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_local_customer,
             customer_id TYPE string,
             name TYPE string,
             email TYPE string,
             active TYPE abap_bool,
           END OF ty_local_customer.
    
    TYPES: tt_local_customers TYPE TABLE OF ty_local_customer WITH DEFAULT KEY.

    METHODS test_local_customer_create
      IMPORTING iv_customer_id TYPE string
                iv_name TYPE string
                iv_email TYPE string
      EXPORTING es_customer TYPE ty_local_customer
                ev_success TYPE abap_bool.

    METHODS test_local_customer_table
      IMPORTING it_customers TYPE tt_local_customers
                iv_filter_active TYPE abap_bool DEFAULT abap_true
      EXPORTING et_filtered_customers TYPE tt_local_customers
                ev_count TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS validate_email
      IMPORTING iv_email TYPE string
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS zcl_test_validation_scenarios IMPLEMENTATION.
  METHOD test_local_customer_create.
    ev_success = abap_false.
    
    IF iv_customer_id IS INITIAL OR iv_name IS INITIAL.
      RETURN.
    ENDIF.
    
    IF iv_email IS NOT INITIAL AND validate_email( iv_email ) = abap_false.
      RETURN.
    ENDIF.
    
    es_customer-customer_id = iv_customer_id.
    es_customer-name = iv_name.
    es_customer-email = iv_email.
    es_customer-active = abap_true.
    
    ev_success = abap_true.
  ENDMETHOD.

  METHOD test_local_customer_table.
    CLEAR: et_filtered_customers, ev_count.
    
    LOOP AT it_customers INTO DATA(ls_customer).
      IF iv_filter_active = abap_true.
        IF ls_customer-active = abap_true.
          APPEND ls_customer TO et_filtered_customers.
        ENDIF.
      ELSE.
        APPEND ls_customer TO et_filtered_customers.
      ENDIF.
    ENDLOOP.
    
    ev_count = lines( et_filtered_customers ).
  ENDMETHOD.

  METHOD validate_email.
    rv_valid = abap_false.
    
    IF iv_email CS '@' AND iv_email CS '.'.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.|.

    " Test 1: Create local customer
    CLEAR lt_params.
    APPEND create_param( iv_name = 'IV_CUSTOMER_ID' iv_direction = 'IMPORTING' 
                        iv_type = 'STRING' iv_value = 'CUST001' ) TO lt_params.
    APPEND create_param( iv_name = 'IV_NAME' iv_direction = 'IMPORTING' 
                        iv_type = 'STRING' iv_value = 'John Doe' ) TO lt_params.
    APPEND create_param( iv_name = 'IV_EMAIL' iv_direction = 'IMPORTING' 
                        iv_type = 'STRING' iv_value = 'john.doe@example.com' ) TO lt_params.
    APPEND create_param( iv_name = 'ES_CUSTOMER' iv_direction = 'EXPORTING' 
                        iv_type = 'ZCL_TEST_VALIDATION_SCENARIOS=>TY_LOCAL_CUSTOMER' iv_value = '' ) TO lt_params.
    APPEND create_param( iv_name = 'EV_SUCCESS' iv_direction = 'EXPORTING' 
                        iv_type = 'ABAP_BOOL' iv_value = '' ) TO lt_params.

    ls_call = create_test_call( iv_method_name = 'TEST_LOCAL_CUSTOMER_CREATE' it_params = lt_params ).

    " Test with hybrid validator
    CREATE OBJECT lo_validator.
    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_TEST_VALIDATION_SCENARIOS'
               class_code = lv_class_source
               input_call = ls_call
      IMPORTING result = ls_result
               message = ls_message
    ).

    log_test_result( iv_test_name = 'Local Types - Customer Create' is_result = ls_result ).

    " Test 2: Process local customer table
    CLEAR lt_params.
    DATA(lv_customers_json) = |[{"customer_id":"CUST001","name":"John Doe","email":"john@example.com","active":"X"},{"customer_id":"CUST002","name":"Jane Smith","email":"jane@example.com","active":""}]|.
    
    APPEND create_param( iv_name = 'IT_CUSTOMERS' iv_direction = 'IMPORTING' 
                        iv_type = 'ZCL_TEST_VALIDATION_SCENARIOS=>TT_LOCAL_CUSTOMERS' iv_value = lv_customers_json ) TO lt_params.
    APPEND create_param( iv_name = 'IV_FILTER_ACTIVE' iv_direction = 'IMPORTING' 
                        iv_type = 'ABAP_BOOL' iv_value = 'X' ) TO lt_params.
    APPEND create_param( iv_name = 'ET_FILTERED_CUSTOMERS' iv_direction = 'EXPORTING' 
                        iv_type = 'ZCL_TEST_VALIDATION_SCENARIOS=>TT_LOCAL_CUSTOMERS' iv_value = '' ) TO lt_params.
    APPEND create_param( iv_name = 'EV_COUNT' iv_direction = 'EXPORTING' 
                        iv_type = 'I' iv_value = '' ) TO lt_params.

    ls_call = create_test_call( iv_method_name = 'TEST_LOCAL_CUSTOMER_TABLE' it_params = lt_params ).

    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_TEST_VALIDATION_SCENARIOS'
               class_code = lv_class_source
               input_call = ls_call
      IMPORTING result = ls_result
               message = ls_message
    ).

    log_test_result( iv_test_name = 'Local Types - Customer Table' is_result = ls_result ).
  ENDMETHOD.

  METHOD test_syntax_validation_only.
    DATA: lo_validator TYPE REF TO zcl_hybrid_method_validator,
          ls_result TYPE zcl_hybrid_method_validator=>ty_validation_result,
          ls_message TYPE zcl_hybrid_method_validator=>ty_message.

    WRITE: / 'Test 4: Syntax Validation Only'.

    " Test with valid syntax
    DATA(lv_valid_class) = |CLASS zcl_test_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS test_method IMPORTING iv_param TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_test_syntax IMPLEMENTATION.
  METHOD test_method.
    " Valid implementation
  ENDMETHOD.
ENDCLASS.|.

    CREATE OBJECT lo_validator.
    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_TEST_SYNTAX'
               class_code = lv_valid_class
               " No input_call = syntax validation only
      IMPORTING result = ls_result
               message = ls_message
    ).

    log_test_result( iv_test_name = 'Syntax Validation Only - Valid' is_result = ls_result ).

    " Test with invalid syntax
    DATA(lv_invalid_class) = |CLASS zcl_test_syntax DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS test_method IMPORTING iv_param TYPE string
  PROTECTED SECTION.  " Missing period after method declaration
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_test_syntax IMPLEMENTATION.
  METHOD test_method.
    " Implementation
  ENDMETHOD.
ENDCLASS.|.

    lo_validator->validate_and_execute(
      EXPORTING class_name = 'ZCL_TEST_SYNTAX'
               class_code = lv_invalid_class
      IMPORTING result = ls_result
               message = ls_message
    ).

    log_test_result( iv_test_name = 'Syntax Validation Only - Invalid' is_result = ls_result ).
  ENDMETHOD.

  METHOD test_complex_json_parameters.
    WRITE: / 'Test 5: Complex JSON Parameters'.
    " This test would be similar to the others but with more complex JSON structures
    " For brevity, showing the concept
    WRITE: / '  (Complex JSON test implementation would go here)'.
  ENDMETHOD.

  METHOD create_test_call.
    rs_call-method_name = iv_method_name.
    rs_call-params = it_params.
  ENDMETHOD.

  METHOD create_param.
    rs_param-name = iv_name.
    rs_param-direction = iv_direction.
    rs_param-type = iv_type.
    rs_param-value = iv_value.
  ENDMETHOD.

  METHOD log_test_result.
    WRITE: / |  { iv_test_name }:|.
    
    IF is_result-success = abap_true.
      WRITE: / |    ✓ SUCCESS - { is_result-message }|.
      IF is_result-used_xco_execution = abap_true.
        WRITE: / |    ✓ Used XCO execution approach|.
        WRITE: / |    ✓ Temp class: { is_result-temp_class_name }|.
      ENDIF.
      WRITE: / |    ✓ Execution time: { is_result-execution_time }ms|.
    ELSE.
      WRITE: / |    ✗ FAILED - { is_result-message }|.
    ENDIF.
    
    WRITE: / |    - Syntax valid: { is_result-syntax_valid }|.
    WRITE: / |    - Method executed: { is_result-method_executed }|.
    WRITE: /.
  ENDMETHOD.

ENDCLASS.
