CLASS zmcp_method_validate DEFINITION PUBLIC.
  PUBLIC SECTION.
    " Use Data Dictionary types
    TYPES: ty_param TYPE zmcp_param,
           ty_param_tab TYPE zmcp_param_tab,
           ty_call TYPE zmcp_call,
           ty_call_tab TYPE zmcp_call_tab,
           ty_message TYPE bapiret2,
           ty_result TYPE zmcp_result,
           ty_result_tab TYPE zmcp_result_tab.

    METHODS run
      IMPORTING class_name TYPE string            " Name of the class (local or global)
                class_code TYPE string
                input_call      TYPE ty_call OPTIONAL
      EXPORTING !result    TYPE zmcp_call  " returns actual outputs
                !message   TYPE ty_message.
PROTECTED SECTION.

PRIVATE SECTION.
  METHODS: is_json_structure_or_table
    IMPORTING iv_value TYPE string
    RETURNING VALUE(rv_is_json) TYPE abap_bool,
    
    assign_json_to_parameter
    IMPORTING iv_json TYPE string
              iv_type TYPE string
    CHANGING  cr_data TYPE REF TO data,
    
    parse_json_structure
    IMPORTING iv_json TYPE string
    CHANGING  cr_structure TYPE any,
    
    parse_json_table
    IMPORTING iv_json TYPE string
    CHANGING  cr_table TYPE any.
ENDCLASS.


CLASS zmcp_method_validate IMPLEMENTATION.
METHOD run.
  DATA abs_type TYPE string.
  DATA source   TYPE STANDARD TABLE OF string.

  IF class_code IS NOT INITIAL.
    SPLIT class_code AT cl_abap_char_utilities=>newline INTO TABLE source.
    INSERT 'PROGRAM ztmp.' INTO source INDEX 1.

    GENERATE SUBROUTINE POOL source NAME FINAL(lv_program)
             MESSAGE FINAL(mess)
             LINE DATA(line).
    IF sy-subrc <> 0.
      message-message = |Line: { line } - { mess }|.
      RETURN.  " syntax error during generation
    ENDIF.
    abs_type = '\PROGRAM=' && lv_program && '\CLASS=' && class_name.
  ELSE.
    abs_type = class_name.
  ENDIF.

  IF input_call-params IS INITIAL.
    RETURN.
  ENDIF.

  DATA(call) = input_call.
  DATA lt_params TYPE abap_parmbind_tab.
  CLEAR lt_params.
  LOOP AT call-params ASSIGNING FIELD-SYMBOL(<lf_param>).
    DATA ls_bind TYPE abap_parmbind.
    ls_bind-name = <lf_param>-name.

    CASE <lf_param>-direction.
      WHEN 'IMPORTING'. ls_bind-kind = cl_abap_objectdescr=>importing.
      WHEN 'EXPORTING'. ls_bind-kind = cl_abap_objectdescr=>exporting.
      WHEN 'CHANGING'. ls_bind-kind = cl_abap_objectdescr=>changing.
      WHEN 'RECEIVING' OR 'RETURNING'. ls_bind-kind = cl_abap_objectdescr=>receiving.
      WHEN OTHERS.
        CLEAR ls_bind-kind.
    ENDCASE.

    CREATE DATA <lf_param>-data TYPE (<lf_param>-type).
    ASSIGN <lf_param>-data->* TO FIELD-SYMBOL(<lf_1>).

    " Enhanced parameter value assignment for structures and tables
    IF is_json_structure_or_table( <lf_param>-value ).
      assign_json_to_parameter( EXPORTING iv_json = <lf_param>-value
                                          iv_type = <lf_param>-type
                                CHANGING  cr_data = <lf_param>-data ).
    ELSE.
      " Simple value assignment for basic types
      <lf_1> = <lf_param>-value.
    ENDIF.
    
    ls_bind-value = <lf_param>-data.
    INSERT ls_bind INTO TABLE lt_params.
  ENDLOOP.

  TRY.
      DATA lo_obj TYPE REF TO object.
      CREATE OBJECT lo_obj TYPE (abs_type).
      CALL METHOD lo_obj->(call-method_name)
        PARAMETER-TABLE lt_params.              " dynamic invocation:contentReference[oaicite:3]{index=3}

    CATCH cx_sy_dyn_call_illegal_method       " method not found"
          cx_sy_dyn_call_illegal_type
          cx_sy_dyn_call_param_missing
          cx_sy_dyn_call_param_not_found INTO DATA(lo_ex).
      DATA(exc_text) = lo_ex->get_text( ).
      message-message = exc_text.
  ENDTRY.

  DATA param_tab TYPE zmcp_param_tab.
  LOOP AT call-params INTO DATA(ls_param) WHERE direction <> 'EXPORTING'.
    READ TABLE lt_params INTO DATA(ls_bind_read) WITH KEY name = ls_param-name.
    IF sy-subrc = 0 AND ls_bind_read-value IS BOUND.
      ASSIGN ls_bind_read-value->* TO FIELD-SYMBOL(<ls_out>).
      APPEND VALUE zmcp_param( name      = ls_param-name
                               direction = ls_param-direction
                               value     = CONV string( <ls_out> ) )
             TO param_tab.
    ENDIF.
  ENDLOOP.

  result-method_name = call-method_name.
  result-params      = param_tab.
ENDMETHOD.

METHOD is_json_structure_or_table.
  " Check if the value looks like JSON (starts with { or [)
  DATA(lv_trimmed) = |{ iv_value }|.
  CONDENSE lv_trimmed.
  
  IF lv_trimmed CP '{*' OR lv_trimmed CP '[*'.
    rv_is_json = abap_true.
  ELSE.
    rv_is_json = abap_false.
  ENDIF.
ENDMETHOD.

METHOD assign_json_to_parameter.
  ASSIGN cr_data->* TO FIELD-SYMBOL(<lf_target>).
  
  " Determine if it's a table or structure based on JSON format
  DATA(lv_trimmed) = |{ iv_json }|.
  CONDENSE lv_trimmed.
  
  IF lv_trimmed CP '[*'.
    " It's a table (JSON array)
    parse_json_table( EXPORTING iv_json = iv_json
                      CHANGING  cr_table = <lf_target> ).
  ELSEIF lv_trimmed CP '{*'.
    " It's a structure (JSON object)
    parse_json_structure( EXPORTING iv_json = iv_json
                          CHANGING  cr_structure = <lf_target> ).
  ELSE.
    " Fallback to simple assignment
    <lf_target> = iv_json.
  ENDIF.
ENDMETHOD.

METHOD parse_json_structure.
  " Simple JSON structure parser
  " This is a basic implementation - in production you might want to use
  " a proper JSON library like /UI2/CL_JSON or similar
  
  DATA: lv_json TYPE string,
        lv_field TYPE string,
        lv_value TYPE string,
        lv_pos TYPE i,
        lv_end_pos TYPE i.
  
  " Remove outer braces and whitespace
  lv_json = iv_json.
  REPLACE ALL OCCURRENCES OF '{' IN lv_json WITH ''.
  REPLACE ALL OCCURRENCES OF '}' IN lv_json WITH ''.
  CONDENSE lv_json.
  
  " Split by comma and process each field
  SPLIT lv_json AT ',' INTO TABLE DATA(lt_pairs).
  
  LOOP AT lt_pairs INTO DATA(lv_pair).
    " Split field:value pair
    SPLIT lv_pair AT ':' INTO lv_field lv_value.
    
    " Clean up field name (remove quotes)
    REPLACE ALL OCCURRENCES OF '"' IN lv_field WITH ''.
    CONDENSE lv_field.
    
    " Clean up value (remove quotes)
    REPLACE ALL OCCURRENCES OF '"' IN lv_value WITH ''.
    CONDENSE lv_value.
    
    " Assign to structure field dynamically
    ASSIGN COMPONENT lv_field OF STRUCTURE cr_structure TO FIELD-SYMBOL(<lf_field>).
    IF sy-subrc = 0.
      <lf_field> = lv_value.
    ENDIF.
  ENDLOOP.
ENDMETHOD.

METHOD parse_json_table.
  " Real JSON table parser
  " Parse JSON array format: [{"field1":"value1"},{"field2":"value2"}]
  " Uses parse_json_structure to populate each table line
  
  DATA: lv_json TYPE string,
        lv_object TYPE string,
        lr_line_ref TYPE REF TO data,
        lo_struct_descr TYPE REF TO cl_abap_structdescr,
        lo_table_descr TYPE REF TO cl_abap_tabledescr.
  
  " Remove outer brackets and whitespace
  lv_json = iv_json.
  REPLACE ALL OCCURRENCES OF '[' IN lv_json WITH ''.
  REPLACE ALL OCCURRENCES OF ']' IN lv_json WITH ''.
  CONDENSE lv_json.
  
  " Skip if empty JSON array
  IF lv_json IS INITIAL.
    RETURN.
  ENDIF.
  
  " Get table description to create line structure
  lo_table_descr ?= cl_abap_typedescr=>describe_by_data( cr_table ).
  lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
  
  " Clear the table first
  CLEAR cr_table.
  
  " Split by },{ to get individual JSON objects
  " Handle nested braces by using a unique separator
  REPLACE ALL OCCURRENCES OF '},{' IN lv_json WITH '}|SEPARATOR|{'.
  SPLIT lv_json AT '|SEPARATOR|' INTO TABLE DATA(lt_objects).
  
  " Process each JSON object
  LOOP AT lt_objects INTO lv_object.
    " Create a new line for the table
    CREATE DATA lr_line_ref TYPE HANDLE lo_struct_descr.
    ASSIGN lr_line_ref->* TO FIELD-SYMBOL(<ls_line>).
    
    " Parse the JSON object into the line structure
    parse_json_structure( EXPORTING iv_json = lv_object
                          CHANGING  cr_structure = <ls_line> ).
    
    " Insert the populated line into the table
    INSERT <ls_line> INTO TABLE cr_table.
  ENDLOOP.
  
ENDMETHOD.

ENDCLASS.