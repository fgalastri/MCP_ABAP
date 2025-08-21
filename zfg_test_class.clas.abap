CLASS zfg_test_class DEFINITION PUBLIC.
  PUBLIC SECTION.
    " parameter definition for one parameter of a method call
    TYPES: BEGIN OF ty_param,
             name      TYPE string,
             direction TYPE string, " IMPORTING/EXPORTING/CHANGING/RECEIVING
             value     TYPE string, " value as text (will be converted implicitly)
           END OF ty_param.
    TYPES ty_param_tab TYPE STANDARD TABLE OF ty_param WITH DEFAULT KEY.

    " definition of a test call (method name and its parameters)
    TYPES: BEGIN OF ty_call,
             method_name TYPE string,
             params      TYPE ty_param_tab,
           END OF ty_call.
    TYPES ty_call_tab TYPE STANDARD TABLE OF ty_call WITH DEFAULT KEY.
    TYPES ty_message  TYPE bapiret2.
    " result of one output parameter after execution
    TYPES: BEGIN OF ty_result,
             method_name TYPE string,
             param_name  TYPE string,
             value       TYPE string,
           END OF ty_result.
    TYPES ty_result_tab TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    METHODS run
      IMPORTING class_name TYPE string            " Name of the class (local or global)
                class_code TYPE string OPTIONAL   " Complete ABAP class implementation (optional)
                calls      TYPE ty_call_tab       " List of test calls with parameters
      EXPORTING !result    TYPE ty_result_tab  " returns actual outputs
                !message   TYPE ty_message.
ENDCLASS.


CLASS zfg_test_class IMPLEMENTATION.
  METHOD run.
    DATA call_result TYPE ty_result_tab.
    DATA abs_type    TYPE string.
    DATA source      TYPE STANDARD TABLE OF string.

    " If source code is provided, generate a temporary program with the class definition"
    IF class_code IS NOT INITIAL.
      SPLIT class_code AT cl_abap_char_utilities=>newline INTO TABLE source.
      " Ensure the program has an introductory PROGRAM statement (required by GENERATE)"
      INSERT 'PROGRAM ztmp.' INTO source INDEX 1.

      " Compile the source code into a subroutine pool"
      GENERATE SUBROUTINE POOL source NAME FINAL(lv_program)
               MESSAGE FINAL(mess).
      IF sy-subrc <> 0.
        message-message = mess.
        RETURN.  " syntax error during generation
      ENDIF.
      " Build the absolute type name for the local class: \PROGRAM=<prog>\CLASS=<class>"
      abs_type = '\PROGRAM=' && lv_program && '\CLASS=' && class_name.
    ELSE.
      " If no source code, assume it's an existing global class"
      abs_type = class_name.
    ENDIF.

    " Loop over calls and execute each method"
    LOOP AT calls INTO DATA(ls_call).
      " Prepare parameter binding table for dynamic call"
      DATA lt_params TYPE abap_parmbind_tab.
      CLEAR lt_params.
      LOOP AT ls_call-params ASSIGNING FIELD-SYMBOL(<lf_param>).
        DATA ls_bind TYPE abap_parmbind.
        ls_bind-name = <lf_param>-name.
        " Map direction text to ABAP kind (optional – can be left initial to skip checks)"
        CASE <lf_param>-direction.
          WHEN 'IMPORTING'. ls_bind-kind = cl_abap_objectdescr=>importing.
          WHEN 'EXPORTING'. ls_bind-kind = cl_abap_objectdescr=>exporting.
          WHEN 'CHANGING'. ls_bind-kind = cl_abap_objectdescr=>changing.
          WHEN 'RECEIVING' OR 'RETURNING'. ls_bind-kind = cl_abap_objectdescr=>receiving.
          WHEN OTHERS.
            CLEAR ls_bind-kind.
        ENDCASE.
        " Create a local variable to hold the value; ABAP will convert types implicitly"
        GET REFERENCE OF <lf_param>-value INTO ls_bind-value.
        INSERT ls_bind INTO TABLE lt_params.
      ENDLOOP.

      TRY.
          " Create instance if calling an instance method; for static calls use => in the method name"
          DATA lo_obj TYPE REF TO object.
          CREATE OBJECT lo_obj TYPE (abs_type).
          " Invoke the method dynamically with parameter table (no static checks)"
          CALL METHOD lo_obj->(ls_call-method_name)
            PARAMETER-TABLE lt_params.              " dynamic invocation:contentReference[oaicite:3]{index=3}

        CATCH cx_sy_dyn_call_illegal_method       " method not found"
              cx_sy_dyn_call_param_not_found INTO DATA(lo_ex).
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA(exc_text) = lo_ex->get_text( ).
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA(exc_long_text) = lo_ex->get_longtext( ).
          " Return empty result or log exception"
      ENDTRY.

      " Collect output values (non-importing parameters)"
      LOOP AT ls_call-params INTO DATA(ls_param) WHERE direction <> 'IMPORTING'.
        READ TABLE lt_params INTO DATA(ls_bind_read) WITH KEY name = ls_param-name.
        IF sy-subrc = 0 AND ls_bind_read-value IS BOUND.
          ASSIGN ls_bind_read-value->* TO FIELD-SYMBOL(<ls_out>).
          APPEND VALUE ty_result( method_name = ls_call-method_name
                                  param_name  = ls_param-name
                                  value       = CONV string( <ls_out> ) )
                 TO call_result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    result = call_result.
  ENDMETHOD.
ENDCLASS.
