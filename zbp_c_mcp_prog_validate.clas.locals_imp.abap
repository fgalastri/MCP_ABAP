CLASS lhc_zbp_c_mcp_prog_validate DEFINITION
  INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    " Type definitions for action parameters using Data Dictionary types
    TYPES: ty_prog_validate_input TYPE zmcp_prog_validate_input,
           ty_prog_validate_result TYPE zmcp_prog_validate_result.

  PRIVATE SECTION.

    METHODS validate_program FOR MODIFY
      IMPORTING keys FOR ACTION ZC_MCP_PROGRAM_VALIDATE~validate_program
      RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ZC_MCP_PROGRAM_VALIDATE RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ ZC_MCP_PROGRAM_VALIDATE RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK ZC_MCP_PROGRAM_VALIDATE.

    METHODS validate_program_syntax
      IMPORTING iv_program_name TYPE string
                iv_source_code  TYPE string
      RETURNING VALUE(rs_result) TYPE ty_prog_validate_result.

ENDCLASS.

CLASS lhc_zbp_c_mcp_prog_validate IMPLEMENTATION.
  METHOD validate_program.
    DATA ls_result TYPE ty_prog_validate_result.

    " Process each key (should be one in most cases)
    LOOP AT keys INTO DATA(ls_key).

      TRY.
          " Get input parameters from the action
          DATA(ls_input) = ls_key-%param.

          " Validate the program syntax
          ls_result = validate_program_syntax( 
            iv_program_name = ls_input-program_name
            iv_source_code = ls_input-source_code ).

        CATCH cx_root INTO DATA(lo_ex).
          ls_result-status = 'ERROR'.
          ls_result-message = 'Validation error: ' && lo_ex->get_text( ).
          ls_result-error_message = lo_ex->get_text( ).
          ls_result-program_name = ls_input-program_name.
          ls_result-request_id = cl_system_uuid=>create_uuid_x16_static( ).

      ENDTRY.

      APPEND VALUE #( %param     = ls_result
                      request_id = ls_key-request_id
                      %cid_ref   = ls_key-%cid_ref )
             TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_program_syntax.
    DATA: lv_temp_program TYPE c LENGTH 40,
          lt_source_lines TYPE TABLE OF string.

    " Initialize result
    rs_result-request_id = cl_system_uuid=>create_uuid_x16_static( ).
    rs_result-program_name = iv_program_name.

    " Generate temporary program name (keep it short)
    DATA(lv_uuid_short) = rs_result-request_id+0(8).
    lv_temp_program = 'ZTEMP' && lv_uuid_short.

    " Convert source code string to internal table
    SPLIT iv_source_code AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_source_lines.

    TRY.
        " Use GENERATE SUBROUTINE POOL to validate syntax
        DATA: lv_message TYPE string,
              lv_line TYPE i.
              
        GENERATE SUBROUTINE POOL lt_source_lines
          NAME lv_temp_program
          MESSAGE lv_message
          LINE lv_line.

        " Check if there were syntax errors
        IF lv_message IS NOT INITIAL OR lv_line > 0.
          rs_result-status = 'ERROR'.
          rs_result-error_message = lv_message.
          rs_result-error_line = lv_line.
          IF lv_message IS INITIAL.
            rs_result-message = 'Program syntax validation failed'.
          ELSE.
            rs_result-message = lv_message.
          ENDIF.
        ELSE.
          " If we reach here, syntax is valid
          rs_result-status = 'SUCCESS'.
          rs_result-message = 'Program syntax validation completed successfully'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-status = 'ERROR'.
        rs_result-message = 'Syntax validation failed: ' && lx_error->get_text( ).
        rs_result-error_message = lx_error->get_text( ).
    ENDTRY.

    " Clean up temporary program if it was created successfully
    IF rs_result-status = 'SUCCESS'.
      TRY.
          DELETE REPORT lv_temp_program.
        CATCH cx_root.
          " Ignore cleanup errors
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.
