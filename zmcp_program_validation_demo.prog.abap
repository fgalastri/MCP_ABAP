*&---------------------------------------------------------------------*
*& Report ZMCP_PROGRAM_VALIDATION_DEMO
*&---------------------------------------------------------------------*
*& Demo program to test ABAP program syntax validation
*&---------------------------------------------------------------------*
REPORT zmcp_program_validation_demo.

PARAMETERS: p_test TYPE i DEFAULT 1.

SELECTION-SCREEN COMMENT /1(60) text-001.
SELECTION-SCREEN COMMENT /1(60) text-002.
SELECTION-SCREEN COMMENT /1(60) text-003.

INITIALIZATION.
  text-001 = 'Test 1: Valid ABAP Program'.
  text-002 = 'Test 2: Invalid ABAP Program (Syntax Error)'.
  text-003 = 'Test 3: Complex ABAP Program'.

START-OF-SELECTION.

  DATA: lo_validator TYPE REF TO zcl_mcp_program_validator,
        ls_result TYPE zcl_mcp_program_validator=>ty_validation_result,
        lv_source_code TYPE string.

  CREATE OBJECT lo_validator.

  CASE p_test.
    WHEN 1.
      " Test with valid ABAP program
      lv_source_code = |REPORT ztest_valid.| && cl_abap_char_utilities=>cr_lf &&
                       |DATA: lv_message TYPE string.| && cl_abap_char_utilities=>cr_lf &&
                       |lv_message = 'Hello World'.| && cl_abap_char_utilities=>cr_lf &&
                       |WRITE: / lv_message.|.

      WRITE: / '=== Testing Valid ABAP Program ==='.
      ls_result = lo_validator->validate_program_syntax(
        iv_program_name = 'ZTEST_VALID'
        iv_source_code = lv_source_code
      ).

    WHEN 2.
      " Test with invalid ABAP program (syntax error)
      lv_source_code = |REPORT ztest_invalid.| && cl_abap_char_utilities=>cr_lf &&
                       |DATA: lv_message TYPE string| && cl_abap_char_utilities=>cr_lf &&  " Missing period
                       |lv_message = 'Hello World'.| && cl_abap_char_utilities=>cr_lf &&
                       |WRITE: / lv_message.|.

      WRITE: / '=== Testing Invalid ABAP Program ==='.
      ls_result = lo_validator->validate_program_syntax(
        iv_program_name = 'ZTEST_INVALID'
        iv_source_code = lv_source_code
      ).

    WHEN 3.
      " Test with complex ABAP program
      lv_source_code = |REPORT ztest_complex.| && cl_abap_char_utilities=>cr_lf &&
                       |TABLES: mara.| && cl_abap_char_utilities=>cr_lf &&
                       |DATA: BEGIN OF ls_material,| && cl_abap_char_utilities=>cr_lf &&
                       |         matnr TYPE mara-matnr,| && cl_abap_char_utilities=>cr_lf &&
                       |         mtart TYPE mara-mtart,| && cl_abap_char_utilities=>cr_lf &&
                       |       END OF ls_material.| && cl_abap_char_utilities=>cr_lf &&
                       |SELECT SINGLE matnr, mtart| && cl_abap_char_utilities=>cr_lf &&
                       |  FROM mara| && cl_abap_char_utilities=>cr_lf &&
                       |  INTO ls_material.| && cl_abap_char_utilities=>cr_lf &&
                       |IF sy-subrc = 0.| && cl_abap_char_utilities=>cr_lf &&
                       |  WRITE: / 'Material:', ls_material-matnr.| && cl_abap_char_utilities=>cr_lf &&
                       |ENDIF.|.

      WRITE: / '=== Testing Complex ABAP Program ==='.
      ls_result = lo_validator->validate_program_syntax(
        iv_program_name = 'ZTEST_COMPLEX'
        iv_source_code = lv_source_code
      ).

    WHEN OTHERS.
      WRITE: / 'Invalid test selection. Please choose 1, 2, or 3.'.
      RETURN.
  ENDCASE.

  " Display results
  WRITE: / 'Program Name:', ls_result-program_name.
  WRITE: / 'Request ID:', ls_result-request_id.
  WRITE: / 'Status:', ls_result-status.
  WRITE: / 'Message:', ls_result-message.
  IF ls_result-error_line > 0.
    WRITE: / 'Error Line:', ls_result-error_line.
  ENDIF.
  IF ls_result-error_message IS NOT INITIAL.
    WRITE: / 'Error Details:', ls_result-error_message.
  ENDIF.
  WRITE: / 'Created At:', ls_result-created_at.

