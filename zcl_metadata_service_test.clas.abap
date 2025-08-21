CLASS zcl_metadata_service_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_metadata_service.

    METHODS setup.
    METHODS teardown.

    " Test methods for get_cds_fields
    METHODS test_cds_single FOR TESTING.
    METHODS test_cds_multiple FOR TESTING.
    METHODS test_cds_empty FOR TESTING.

    " Test methods for get_table_stru_fields
    METHODS test_table_single FOR TESTING.
    METHODS test_table_multiple FOR TESTING.
    METHODS test_table_empty FOR TESTING.

    " Test methods for get_classes_methods
    METHODS test_class_single FOR TESTING.
    METHODS test_class_multiple FOR TESTING.
    METHODS test_class_empty FOR TESTING.

    " Test methods for get_functions
    METHODS test_func_single FOR TESTING.
    METHODS test_func_multiple FOR TESTING.
    METHODS test_func_empty FOR TESTING.

    " Helper methods
    METHODS assert_cds_field
      IMPORTING
        is_field TYPE zcl_metadata_service=>ty_cds_field
        iv_expected_name TYPE zcl_metadata_service=>ty_cds_name.

    METHODS assert_table_field
      IMPORTING
        is_field TYPE zcl_metadata_service=>ty_table_field
        iv_expected_name TYPE zcl_metadata_service=>ty_field_name.

    METHODS assert_method_param
      IMPORTING
        is_param TYPE zcl_metadata_service=>ty_method_param
        iv_expected_name TYPE zcl_metadata_service=>ty_class_name.

    METHODS assert_function_param
      IMPORTING
        is_param TYPE zcl_metadata_service=>ty_function_param
        iv_expected_name TYPE zcl_metadata_service=>ty_func_name.
ENDCLASS.

CLASS zcl_metadata_service_test IMPLEMENTATION.

  METHOD setup.
    " Create instance of class under test
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD teardown.
    " Clean up
    CLEAR mo_cut.
  ENDMETHOD.

  "------------------------------------------------------------------------------
  "* Tests for get_cds_fields method
  "------------------------------------------------------------------------------
  METHOD test_cds_single.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_cds_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_cds_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_cds_field_tab.

    ls_input = 'ZV_TEST_CDS'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_cds_fields( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1
      msg = 'Should return exactly one CDS field record'
    ).

    READ TABLE lt_result INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_field>).
    assert_cds_field(
      is_field = <ls_field>
      iv_expected_name = 'ZV_TEST_CDS'
    ).
  ENDMETHOD.

  METHOD test_cds_multiple.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_cds_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_cds_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_cds_field_tab.

    ls_input = 'ZV_CDS_ONE'.
    APPEND ls_input TO lt_input.
    ls_input = 'ZV_CDS_TWO'.
    APPEND ls_input TO lt_input.
    ls_input = 'ZV_CDS_THREE'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_cds_fields( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 3
      msg = 'Should return three CDS field records'
    ).

    " Verify first record
    READ TABLE lt_result INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_field1>).
    assert_cds_field(
      is_field = <ls_field1>
      iv_expected_name = 'ZV_CDS_ONE'
    ).

    " Verify third record
    READ TABLE lt_result INDEX 3 ASSIGNING FIELD-SYMBOL(<ls_field3>).
    assert_cds_field(
      is_field = <ls_field3>
      iv_expected_name = 'ZV_CDS_THREE'
    ).
  ENDMETHOD.

  METHOD test_cds_empty.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_cds_name_tab.
    DATA lt_result TYPE zcl_metadata_service=>ty_cds_field_tab.

    " When
    lt_result = mo_cut->get_cds_fields( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 0
      msg = 'Should return empty table for empty input'
    ).
  ENDMETHOD.

  "------------------------------------------------------------------------------
  "* Tests for get_table_stru_fields method
  "------------------------------------------------------------------------------
  METHOD test_table_single.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_field_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_field_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_table_field_tab.

    ls_input = 'ZTABLE_TEST'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_table_stru_fields( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1
      msg = 'Should return exactly one table field record'
    ).

    READ TABLE lt_result INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_field>).
    assert_table_field(
      is_field = <ls_field>
      iv_expected_name = 'ZTABLE_TEST'
    ).
  ENDMETHOD.

  METHOD test_table_multiple.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_field_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_field_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_table_field_tab.

    ls_input = 'MARA'.
    APPEND ls_input TO lt_input.
    ls_input = 'MARC'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_table_stru_fields( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 2
      msg = 'Should return two table field records'
    ).
  ENDMETHOD.

  METHOD test_table_empty.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_field_name_tab.
    DATA lt_result TYPE zcl_metadata_service=>ty_table_field_tab.

    " When
    lt_result = mo_cut->get_table_stru_fields( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 0
      msg = 'Should return empty table for empty input'
    ).
  ENDMETHOD.

  "------------------------------------------------------------------------------
  "* Tests for get_classes_methods method
  "------------------------------------------------------------------------------
  METHOD test_class_single.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_class_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_class_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_method_param_tab.

    ls_input = 'ZCL_TEST_CLASS'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_classes_methods( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1
      msg = 'Should return exactly one method parameter record'
    ).

    READ TABLE lt_result INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_param>).
    assert_method_param(
      is_param = <ls_param>
      iv_expected_name = 'ZCL_TEST_CLASS'
    ).
  ENDMETHOD.

  METHOD test_class_multiple.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_class_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_class_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_method_param_tab.

    ls_input = 'ZCL_CLASS_ONE'.
    APPEND ls_input TO lt_input.
    ls_input = 'ZCL_CLASS_TWO'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_classes_methods( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 2
      msg = 'Should return two method parameter records'
    ).
  ENDMETHOD.

  METHOD test_class_empty.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_class_name_tab.
    DATA lt_result TYPE zcl_metadata_service=>ty_method_param_tab.

    " When
    lt_result = mo_cut->get_classes_methods( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 0
      msg = 'Should return empty table for empty input'
    ).
  ENDMETHOD.

  "------------------------------------------------------------------------------
  "* Tests for get_functions method
  "------------------------------------------------------------------------------
  METHOD test_func_single.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_func_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_func_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_function_param_tab.

    ls_input = 'Z_TEST_FUNCTION'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_functions( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1
      msg = 'Should return exactly one function parameter record'
    ).

    READ TABLE lt_result INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_param>).
    assert_function_param(
      is_param = <ls_param>
      iv_expected_name = 'Z_TEST_FUNCTION'
    ).
  ENDMETHOD.

  METHOD test_func_multiple.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_func_name_tab.
    DATA ls_input TYPE zcl_metadata_service=>ty_func_name.
    DATA lt_result TYPE zcl_metadata_service=>ty_function_param_tab.

    ls_input = 'FUNC_ONE'.
    APPEND ls_input TO lt_input.
    ls_input = 'FUNC_TWO'.
    APPEND ls_input TO lt_input.
    ls_input = 'FUNC_THREE'.
    APPEND ls_input TO lt_input.

    " When
    lt_result = mo_cut->get_functions( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 3
      msg = 'Should return three function parameter records'
    ).
  ENDMETHOD.

  METHOD test_func_empty.
    " Given
    DATA lt_input TYPE zcl_metadata_service=>ty_func_name_tab.
    DATA lt_result TYPE zcl_metadata_service=>ty_function_param_tab.

    " When
    lt_result = mo_cut->get_functions( lt_input ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 0
      msg = 'Should return empty table for empty input'
    ).
  ENDMETHOD.

  "------------------------------------------------------------------------------
  "* Helper methods for assertions
  "------------------------------------------------------------------------------
  METHOD assert_cds_field.
    " Verify CDS field structure and default values
    cl_abap_unit_assert=>assert_equals(
      act = is_field-cds_name
      exp = iv_expected_name
      msg = 'CDS name should match input'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-sql_view_name
      exp = iv_expected_name
      msg = 'SQL view name should match CDS name for sample data'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-field_position
      exp = 1
      msg = 'Field position should be 1'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-field_name
      exp = 'SAMPLE_FIELD'
      msg = 'Field name should be SAMPLE_FIELD'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-key_flag
      exp = 'X'
      msg = 'Key flag should be X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-abap_dtype
      exp = 'C'
      msg = 'ABAP data type should be C'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-db_dtype
      exp = 'CHAR'
      msg = 'DB data type should be CHAR'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-length
      exp = 30
      msg = 'Length should be 30'
    ).
  ENDMETHOD.

  METHOD assert_table_field.
    " Verify table field structure and default values
    cl_abap_unit_assert=>assert_equals(
      act = is_field-tab_name
      exp = iv_expected_name
      msg = 'Table name should match input'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-field_name
      exp = 'SAMPLE_FIELD'
      msg = 'Field name should be SAMPLE_FIELD'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-key_flag
      exp = 'X'
      msg = 'Key flag should be X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-abap_dtype
      exp = 'C'
      msg = 'ABAP data type should be C'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_field-length
      exp = 30
      msg = 'Length should be 30'
    ).
  ENDMETHOD.

  METHOD assert_method_param.
    " Verify method parameter structure and default values
    cl_abap_unit_assert=>assert_equals(
      act = is_param-cls_name
      exp = iv_expected_name
      msg = 'Class name should match input'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_param-method_name
      exp = 'SAMPLE_METHOD'
      msg = 'Method name should be SAMPLE_METHOD'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_param-param_name
      exp = 'IV_INPUT'
      msg = 'Parameter name should be IV_INPUT'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_param-param_kind
      exp = 'I'
      msg = 'Parameter kind should be I (importing)'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_param-abap_dtype
      exp = 'STRING'
      msg = 'ABAP data type should be STRING'
    ).
  ENDMETHOD.

  METHOD assert_function_param.
    " Verify function parameter structure and default values
    cl_abap_unit_assert=>assert_equals(
      act = is_param-func_name
      exp = iv_expected_name
      msg = 'Function name should match input'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_param-param_name
      exp = 'IV_INPUT'
      msg = 'Parameter name should be IV_INPUT'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = is_param-param_type
      exp = 'I'
      msg = 'Parameter type should be I (importing)'
    ).

    cl_abap_unit_assert=>assert_not_initial(
      act = is_param-func_name
      msg = 'Function name should not be initial'
    ).
  ENDMETHOD.

ENDCLASS.
