CLASS lhc_zbp_c_mcp_method_validate DEFINITION
  INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    " Type definitions for action parameters using Data Dictionary types
    TYPES: ty_validate_input TYPE zmcp_validate_input,
           ty_validate_result TYPE zmcp_validate_result,
           ty_execute_input TYPE zmcp_execute_input,
           ty_execute_result TYPE zmcp_execute_result.

  PRIVATE SECTION.

    METHODS validate_methods FOR MODIFY
      IMPORTING keys FOR ACTION ZC_MCP_METHOD_VALIDATE~validate_methods
      RESULT result.

    METHODS execute_calls FOR MODIFY
      IMPORTING keys FOR ACTION ZC_MCP_METHOD_VALIDATE~execute_calls
      RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ZC_MCP_METHOD_VALIDATE RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ ZC_MCP_METHOD_VALIDATE RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK ZC_MCP_METHOD_VALIDATE.

    METHODS convert_json_to_calls
      IMPORTING iv_json_calls     TYPE string
      RETURNING VALUE(rt_calls)   TYPE zmcp_call
      RAISING   cx_sy_conversion_error.

    METHODS convert_results_to_json
      IMPORTING it_results        TYPE zmcp_call
      RETURNING VALUE(rv_json)    TYPE string.

ENDCLASS.

CLASS lhc_zbp_c_mcp_method_validate IMPLEMENTATION.
  METHOD validate_methods.
    DATA ls_result    TYPE ty_validate_result.
    DATA lo_validator TYPE REF TO zmcp_method_validate.

    " Process each key (should be one in most cases)
    LOOP AT keys INTO DATA(ls_key).

      " Generate unique request ID
      TRY.
          " Get input parameters from the action
          DATA(ls_input) = ls_key-%param.

          " Create validator instance
          lo_validator = NEW #( ).

          " Convert JSON method calls to internal structure
          DATA(call) = convert_json_to_calls( ls_input-method_calls ).

          " Validate the method calls (dry run)
          lo_validator->run( EXPORTING class_name = ls_input-class_name
                                       class_code = ls_input-class_code
                                       input_call       = call
                             IMPORTING result     = DATA(call_result)
                                       message    = DATA(ls_message) ).

          " Prepare result
          IF ls_message-message IS INITIAL.
            ls_result-status      = 'SUCCESS'.
            ls_result-call_result = convert_results_to_json( call_result ).
            ls_result-message     = 'Validation completed successfully'.
          ELSE.
            ls_result-status  = 'ERROR'.
            ls_result-message = ls_message-message.
          ENDIF.

        CATCH cx_sy_conversion_error INTO DATA(lo_conv_ex).
          ls_result-status  = 'ERROR'.
          ls_result-message = |JSON conversion error: { lo_conv_ex->get_text( ) }|.

        CATCH cx_root INTO DATA(lo_ex).
          ls_result-status  = 'ERROR'.
          ls_result-message = |Validation error: { lo_ex->get_text( ) }|.

      ENDTRY.
      APPEND VALUE #( %param     = ls_result
                      request_id = ls_key-request_id
                      %cid_ref   = ls_key-%cid_ref )
             TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD execute_calls.
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA ls_result    TYPE ty_execute_result.
    DATA lo_validator TYPE REF TO zmcp_method_validate.

    " Process each key
    LOOP AT keys INTO DATA(ls_key).

      " Generate unique request ID
      ls_result-request_id = cl_system_uuid=>create_uuid_x16_static( ).

      TRY.
          " Get input parameters
          DATA(ls_input) = ls_key-%param.

          " Create validator instance
          lo_validator = NEW #( ).

          " Convert JSON method calls to internal structure
          DATA(call) = convert_json_to_calls( ls_input-method_calls ).

          " Execute the method calls
          lo_validator->run( EXPORTING class_name = ls_input-class_name
                                       class_code = ls_input-class_code
                                       input_call = call
                             IMPORTING result     = DATA(lt_results)
                                       message    = DATA(ls_message) ).

          " Prepare result
          IF ls_message-message IS INITIAL.
            ls_result-status        = 'SUCCESS'.
            ls_result-call_result   = convert_results_to_json( lt_results ).
            ls_result-message       = 'Execution completed successfully'.
            ls_result-execution_log = |Executed method call|.
          ELSE.
            ls_result-status        = 'ERROR'.
            ls_result-message       = ls_message-message.
            ls_result-execution_log = 'Execution failed'.
          ENDIF.

        CATCH cx_sy_conversion_error INTO DATA(lo_conv_ex).
          ls_result-status  = 'ERROR'.
          ls_result-message = |JSON conversion error: { lo_conv_ex->get_text( ) }|.

        CATCH cx_root INTO DATA(lo_ex).
          ls_result-status  = 'ERROR'.
          ls_result-message = |Execution error: { lo_ex->get_text( ) }|.

      ENDTRY.

*      " Add result to response
*      APPEND VALUE #( %cid = ls_key-%cid %param = ls_result ) TO result.

    ENDLOOP.
  ENDMETHOD.

  METHOD convert_json_to_calls.
    /ui2/cl_json=>deserialize( EXPORTING json = iv_json_calls
                               CHANGING  data = rt_calls ).
  ENDMETHOD.

  METHOD convert_results_to_json.
    rv_json = /ui2/cl_json=>serialize( data = it_results ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.
