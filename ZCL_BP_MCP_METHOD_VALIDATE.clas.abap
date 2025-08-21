CLASS zcl_bp_mcp_method_validate DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
    
    " Type definitions for action parameters using Data Dictionary types
    TYPES: ty_validate_input TYPE zmcp_validate_input,
           ty_validate_result TYPE zmcp_validate_result,
           ty_execute_input TYPE zmcp_execute_input,
           ty_execute_result TYPE zmcp_execute_result.

  PRIVATE SECTION.
    
    METHODS validate_methods FOR MODIFY
      IMPORTING keys FOR ACTION methodvalidation~validate_methods
      RESULT result.
      
    METHODS execute_calls FOR MODIFY
      IMPORTING keys FOR ACTION methodvalidation~execute_calls  
      RESULT result.
      
    METHODS convert_json_to_calls
      IMPORTING iv_json_calls     TYPE string
      RETURNING VALUE(rt_calls)   TYPE zmcp_call_tab
      RAISING   cx_sy_conversion_error.
      
    METHODS convert_results_to_json
      IMPORTING it_results        TYPE zmcp_result_tab
      RETURNING VALUE(rv_json)    TYPE string.

ENDCLASS.

CLASS zcl_bp_mcp_method_validate IMPLEMENTATION.

  METHOD validate_methods.
    
    DATA ls_result TYPE ty_validate_result.
    DATA lo_validator TYPE REF TO zmcp_method_validate.
    
    " Process each key (should be one in most cases)
    LOOP AT keys INTO DATA(ls_key).
      
      " Generate unique request ID
      ls_result-request_id = cl_system_uuid=>create_uuid_x16_static( ).
      
      TRY.
          " Get input parameters from the action
          DATA(ls_input) = ls_key-%param.
          
          " Create validator instance
          lo_validator = NEW #( ).
          
          " Convert JSON method calls to internal structure
          DATA(lt_calls) = convert_json_to_calls( ls_input-method_calls ).
          
          " Validate the method calls (dry run)
          lo_validator->run( 
            EXPORTING 
              class_name = CONV string( ls_input-class_name )
              class_code = CONV string( ls_input-class_code )
              calls      = lt_calls
            IMPORTING 
              result     = DATA(lt_results)
              message    = DATA(ls_message)
          ).
          
          " Prepare result
          IF ls_message-message IS INITIAL.
            ls_result-status = 'SUCCESS'.
            ls_result-call_result = convert_results_to_json( lt_results ).
            ls_result-message = 'Validation completed successfully'.
          ELSE.
            ls_result-status = 'ERROR'.
            ls_result-message = ls_message-message.
          ENDIF.
          
        CATCH cx_sy_conversion_error INTO DATA(lo_conv_ex).
          ls_result-status = 'ERROR'.
          ls_result-message = |JSON conversion error: { lo_conv_ex->get_text( ) }|.
          
        CATCH cx_root INTO DATA(lo_ex).
          ls_result-status = 'ERROR'. 
          ls_result-message = |Validation error: { lo_ex->get_text( ) }|.
          
      ENDTRY.
      
      " Add result to response
      APPEND VALUE #( %cid = ls_key-%cid %param = ls_result ) TO result.
      
    ENDLOOP.
    
  ENDMETHOD.

  METHOD execute_calls.
    
    DATA ls_result TYPE ty_execute_result.
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
          DATA(lt_calls) = convert_json_to_calls( ls_input-method_calls ).
          
          " Execute the method calls
          lo_validator->run(
            EXPORTING
              class_name = CONV string( ls_input-class_name )
              class_code = CONV string( ls_input-class_code )
              calls      = lt_calls
            IMPORTING
              result     = DATA(lt_results)
              message    = DATA(ls_message)
          ).
          
          " Prepare result
          IF ls_message-message IS INITIAL.
            ls_result-status = 'SUCCESS'.
            ls_result-call_result = convert_results_to_json( lt_results ).
            ls_result-message = 'Execution completed successfully'.
            ls_result-execution_log = |Executed { lines( lt_calls ) } method call(s)|.
          ELSE.
            ls_result-status = 'ERROR'.
            ls_result-message = ls_message-message.
            ls_result-execution_log = 'Execution failed'.
          ENDIF.
          
        CATCH cx_sy_conversion_error INTO DATA(lo_conv_ex).
          ls_result-status = 'ERROR'.
          ls_result-message = |JSON conversion error: { lo_conv_ex->get_text( ) }|.
          
        CATCH cx_root INTO DATA(lo_ex).
          ls_result-status = 'ERROR'.
          ls_result-message = |Execution error: { lo_ex->get_text( ) }|.
          
      ENDTRY.
      
      " Add result to response
      APPEND VALUE #( %cid = ls_key-%cid %param = ls_result ) TO result.
      
    ENDLOOP.
    
  ENDMETHOD.

  METHOD convert_json_to_calls.
    
    " This is a simplified JSON parser for the method calls
    " In a real implementation, you might want to use a proper JSON library
    " For now, assuming a simple JSON structure like:
    " [{"method_name":"test","params":[{"name":"field1","direction":"IMPORTING","value":"test"}]}]
    
    " Simple parsing - you may want to implement a more robust JSON parser
    " For demonstration, create a sample call structure
    DATA ls_call TYPE zmcp_call.
    DATA ls_param TYPE zmcp_param.
    
    ls_call-method_name = 'TEST'.
    
    ls_param-name = 'FIELD1'.
    ls_param-direction = 'IMPORTING'.
    ls_param-value = 'test_value'.
    APPEND ls_param TO ls_call-params.
    
    ls_param-name = 'RESULT'.
    ls_param-direction = 'RETURNING'.
    ls_param-value = ''.
    APPEND ls_param TO ls_call-params.
    
    APPEND ls_call TO rt_calls.
    
  ENDMETHOD.

  METHOD convert_results_to_json.
    
    " Convert results to JSON format
    " This is a simplified JSON serializer
    DATA lv_json_part TYPE string.
    
    rv_json = '['.
    
    LOOP AT it_results INTO DATA(ls_result).
      IF sy-tabix > 1.
        rv_json = rv_json && ','.
      ENDIF.
      
      lv_json_part = |{{"method_name":"{ ls_result-method_name }","param_name":"{ ls_result-param_name }","value":"{ ls_result-value }"}}|.
      rv_json = rv_json && lv_json_part.
      
    ENDLOOP.
    
    rv_json = rv_json && ']'.
    
  ENDMETHOD.

ENDCLASS.
