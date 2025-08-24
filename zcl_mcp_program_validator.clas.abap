CLASS zcl_mcp_program_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_validation_result,
             request_id    TYPE string,
             status        TYPE string,
             message       TYPE string,
             program_name  TYPE string,
             error_line    TYPE i,
             error_message TYPE string,
             created_at    TYPE timestampl,
           END OF ty_validation_result.

    METHODS validate_program_syntax
      IMPORTING
        iv_program_name TYPE string
        iv_source_code  TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_validation_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS generate_request_id
      RETURNING
        VALUE(rv_request_id) TYPE string.

    METHODS extract_error_info
      IMPORTING
        ix_error TYPE REF TO cx_root
      EXPORTING
        ev_line  TYPE i
        ev_message TYPE string.

ENDCLASS.

CLASS zcl_mcp_program_validator IMPLEMENTATION.

  METHOD validate_program_syntax.
    DATA: lv_temp_program TYPE c LENGTH 40,
          lv_error_line   TYPE i,
          lv_error_msg    TYPE string,
          lt_source_lines TYPE TABLE OF string.

    " Initialize result
    rs_result-request_id = generate_request_id( ).
    rs_result-program_name = iv_program_name.
    GET TIME STAMP FIELD rs_result-created_at.

    " Generate temporary program name
    lv_temp_program = |ZTEMP_{ rs_result-request_id+0(8) }|.

    " Convert source code string to internal table
    SPLIT iv_source_code AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_source_lines.

    TRY.
        " Use GENERATE SUBROUTINE POOL to validate syntax
        GENERATE SUBROUTINE POOL lt_source_lines
          NAME lv_temp_program
          MESSAGE rs_result-message
          LINE rs_result-error_line.

        " Check if there were syntax errors
        IF rs_result-message IS NOT INITIAL OR rs_result-error_line > 0.
          rs_result-status = 'ERROR'.
          rs_result-error_message = rs_result-message.
          IF rs_result-message IS INITIAL.
            rs_result-message = 'Program syntax validation failed'.
          ENDIF.
        ELSE.
          " If we reach here, syntax is valid
          rs_result-status = 'SUCCESS'.
          rs_result-message = 'Program syntax validation completed successfully'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-status = 'ERROR'.
        rs_result-message = 'Syntax validation failed: ' && lx_error->get_text( ).
        extract_error_info(
          EXPORTING ix_error = lx_error
          IMPORTING ev_line = rs_result-error_line
                    ev_message = rs_result-error_message
        ).
    ENDTRY.

    " Clean up temporary program if it was created
    IF rs_result-status = 'SUCCESS'.
      TRY.
          DELETE REPORT lv_temp_program.
        CATCH cx_root.
          " Ignore cleanup errors
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD generate_request_id.
    DATA: lv_guid TYPE guid_16.

    TRY.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = lv_guid.
        rv_request_id = lv_guid.
      CATCH cx_root.
        " Fallback to timestamp-based ID
        GET TIME STAMP FIELD DATA(lv_timestamp).
        rv_request_id = |{ lv_timestamp }|.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_error_info.
    " Extract line number and error message from exception
    ev_line = 0.
    ev_message = ix_error->get_text( ).

    " Try to extract line number from message if available
    FIND REGEX 'Line\s+(\d+)' IN ev_message
      SUBMATCHES DATA(lv_line_str).
    IF sy-subrc = 0.
      ev_line = lv_line_str.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
