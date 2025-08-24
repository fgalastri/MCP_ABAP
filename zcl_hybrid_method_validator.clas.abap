CLASS zcl_hybrid_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Use same Data Dictionary types as original class
    TYPES: ty_param TYPE zmcp_param,
           ty_param_tab TYPE zmcp_param_tab,
           ty_call TYPE zmcp_call,
           ty_call_tab TYPE zmcp_call_tab,
           ty_message TYPE bapiret2,
           ty_result TYPE zmcp_result,
           ty_result_tab TYPE zmcp_result_tab.

    " Enhanced result structure
    TYPES: BEGIN OF ty_validation_result,
             success TYPE abap_bool,
             syntax_valid TYPE abap_bool,
             method_executed TYPE abap_bool,
             used_xco_execution TYPE abap_bool,
             temp_class_name TYPE string,
             execution_time TYPE i,
             message TYPE string,
             result TYPE zmcp_call,
           END OF ty_validation_result.

    " Main method - hybrid approach
    METHODS validate_and_execute
      IMPORTING class_name TYPE string
                class_code TYPE string
                input_call TYPE ty_call OPTIONAL
      EXPORTING result TYPE ty_validation_result
                message TYPE ty_message.

    " Drop-in replacement for original class interface
    METHODS run
      IMPORTING class_name TYPE string
                class_code TYPE string
                input_call TYPE ty_call OPTIONAL
      EXPORTING !result TYPE zmcp_call
                !message TYPE ty_message.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Syntax validation using GENERATE SUBROUTINE POOL (fast & simple)
    METHODS validate_syntax_with_pool
      IMPORTING iv_class_code TYPE string
                iv_class_name TYPE string
      EXPORTING ev_success TYPE abap_bool
                ev_message TYPE string
                ev_abs_type TYPE string.

    " Method execution using XCO (handles all complexities)
    METHODS execute_method_with_xco
      IMPORTING iv_class_code TYPE string
                iv_class_name TYPE string
                is_call TYPE ty_call
      EXPORTING ev_success TYPE abap_bool
                ev_temp_class_name TYPE string
                ev_result TYPE zmcp_call
                ev_message TYPE string.

    " XCO-based method execution components
    METHODS create_temp_class_for_execution
      IMPORTING iv_class_code TYPE string
                iv_class_name TYPE string
      EXPORTING ev_temp_class_name TYPE string
                ev_success TYPE abap_bool
                ev_message TYPE string.

    METHODS build_executable_class_source
      IMPORTING iv_original_class_code TYPE string
                iv_original_class_name TYPE string
      EXPORTING ev_class_source TYPE string.

    METHODS extract_complete_class_structure
      IMPORTING iv_class_code TYPE string
      EXPORTING ev_types_section TYPE string
                ev_methods_section TYPE string
                ev_implementation_section TYPE string.

    METHODS execute_on_temp_class
      IMPORTING iv_temp_class_name TYPE string
                is_call TYPE ty_call
      EXPORTING ev_success TYPE abap_bool
                ev_result TYPE zmcp_call
                ev_message TYPE string.

    METHODS cleanup_temp_class
      IMPORTING iv_temp_class_name TYPE string.

    METHODS generate_unique_class_name
      EXPORTING ev_class_name TYPE string.

    " Parameter handling methods (from original class)
    METHODS build_parameter_table
      IMPORTING is_call TYPE ty_call
      EXPORTING et_params TYPE abap_parmbind_tab
                ev_success TYPE abap_bool
                ev_message TYPE string.

    METHODS extract_results_from_params
      IMPORTING is_call TYPE ty_call
                it_params TYPE abap_parmbind_tab
      EXPORTING es_result TYPE zmcp_call.

    " JSON handling methods (from original class)
    METHODS is_json_structure_or_table
      IMPORTING iv_value TYPE string
      RETURNING VALUE(rv_is_json) TYPE abap_bool.

    METHODS assign_json_to_parameter
      IMPORTING iv_json TYPE string
                iv_type TYPE string
      CHANGING cr_data TYPE REF TO data.

    METHODS parse_json_structure
      IMPORTING iv_json TYPE string
      CHANGING cr_structure TYPE any.

    METHODS parse_json_table
      IMPORTING iv_json TYPE string
      CHANGING cr_table TYPE any.
ENDCLASS.

CLASS zcl_hybrid_validator IMPLEMENTATION.

  METHOD validate_and_execute.
    DATA: lv_start_time TYPE timestampl,
          lv_end_time TYPE timestampl,
          lv_syntax_success TYPE abap_bool,
          lv_syntax_message TYPE string,
          lv_abs_type TYPE string,
          lv_exec_success TYPE abap_bool,
          lv_exec_result TYPE zmcp_call,
          lv_exec_message TYPE string,
          lv_temp_class_name TYPE string.

    GET TIME STAMP FIELD lv_start_time.

    " Initialize result
    CLEAR result.
    result-success = abap_false.

    TRY.
        " Step 1: Fast syntax validation using GENERATE SUBROUTINE POOL
        validate_syntax_with_pool(
          EXPORTING iv_class_code = class_code
                   iv_class_name = class_name
          IMPORTING ev_success = lv_syntax_success
                   ev_message = lv_syntax_message
                   ev_abs_type = lv_abs_type
        ).

        result-syntax_valid = lv_syntax_success.

        IF lv_syntax_success = abap_false.
          result-message = |Syntax Error: { lv_syntax_message }|.
          message-message = result-message.
          RETURN.
        ENDIF.

        " Step 2: Check if method execution is requested
        IF input_call IS INITIAL OR input_call-params IS INITIAL.
          result-success = abap_true.
          result-message = 'Syntax validation successful. No method execution requested.'.
          message-message = result-message.
          RETURN.
        ENDIF.

        " Step 3: Always use XCO approach for method execution
        " This handles all parameter complexities including local types
        execute_method_with_xco(
          EXPORTING iv_class_code = class_code
                   iv_class_name = class_name
                   is_call = input_call
          IMPORTING ev_success = lv_exec_success
                   ev_temp_class_name = lv_temp_class_name
                   ev_result = lv_exec_result
                   ev_message = lv_exec_message
        ).

        result-method_executed = lv_exec_success.
        result-used_xco_execution = abap_true.
        result-temp_class_name = lv_temp_class_name.
        result-result = lv_exec_result.

        IF lv_exec_success = abap_true.
          result-success = abap_true.
          result-message = 'Syntax validation and method execution successful.'.
        ELSE.
          result-message = |Method execution failed: { lv_exec_message }|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        result-message = |Unexpected error: { lx_error->get_text( ) }|.
    ENDTRY.

    GET TIME STAMP FIELD lv_end_time.
    result-execution_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_start_time
    ).

    message-message = result-message.
  ENDMETHOD.

  METHOD run.
    " Drop-in replacement for original class interface
    DATA: ls_enhanced_result TYPE ty_validation_result,
          ls_message TYPE ty_message.

    validate_and_execute(
      EXPORTING class_name = class_name
               class_code = class_code
               input_call = input_call
      IMPORTING result = ls_enhanced_result
               message = ls_message
    ).

    " Map to original interface
    result = ls_enhanced_result-result.
    message = ls_message.
  ENDMETHOD.

  METHOD validate_syntax_with_pool.
    " Fast syntax validation using GENERATE SUBROUTINE POOL (same as original)
    DATA: source TYPE STANDARD TABLE OF string,
          lv_program TYPE string,
          lv_mess TYPE string,
          lv_line TYPE i.

    ev_success = abap_false.

    IF iv_class_code IS NOT INITIAL.
      SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE source.
      INSERT 'PROGRAM ztmp.' INTO source INDEX 1.

      GENERATE SUBROUTINE POOL source NAME lv_program
               MESSAGE lv_mess
               LINE lv_line.

      IF sy-subrc <> 0.
        ev_message = |Line: { lv_line } - { lv_mess }|.
        RETURN.
      ENDIF.

      ev_abs_type = '\PROGRAM=' && lv_program && '\CLASS=' && iv_class_name.
      ev_success = abap_true.
      ev_message = 'Syntax validation successful.'.
    ELSE.
      ev_abs_type = iv_class_name.
      ev_success = abap_true.
      ev_message = 'Using existing class name.'.
    ENDIF.
  ENDMETHOD.

  METHOD execute_method_with_xco.
    " Always use XCO approach for method execution
    DATA: lv_temp_success TYPE abap_bool,
          lv_temp_message TYPE string,
          lv_exec_success TYPE abap_bool,
          lv_exec_result TYPE zmcp_call,
          lv_exec_message TYPE string.

    ev_success = abap_false.

    " Step 1: Create temporary class with XCO for execution
    create_temp_class_for_execution(
      EXPORTING iv_class_code = iv_class_code
               iv_class_name = iv_class_name
      IMPORTING ev_temp_class_name = ev_temp_class_name
               ev_success = lv_temp_success
               ev_message = lv_temp_message
    ).

    IF lv_temp_success = abap_false.
      ev_message = |Failed to create temporary class for execution: { lv_temp_message }|.
      RETURN.
    ENDIF.

    " Step 2: Execute method on temporary class
    execute_on_temp_class(
      EXPORTING iv_temp_class_name = ev_temp_class_name
               is_call = is_call
      IMPORTING ev_success = lv_exec_success
               ev_result = lv_exec_result
               ev_message = lv_exec_message
    ).

    " Step 3: Cleanup temporary class
    cleanup_temp_class( ev_temp_class_name ).

    " Set results
    ev_success = lv_exec_success.
    ev_result = lv_exec_result.
    ev_message = lv_exec_message.
  ENDMETHOD.

  METHOD create_temp_class_for_execution.
    " Create temporary class using XCO for method execution
    DATA: lv_class_source TYPE string.

    ev_success = abap_false.

    " Generate unique temporary class name
    generate_unique_class_name(
      IMPORTING ev_class_name = ev_temp_class_name
    ).

    " Build executable class source
    build_executable_class_source(
      EXPORTING iv_original_class_code = iv_class_code
               iv_original_class_name = iv_class_name
      IMPORTING ev_class_source = lv_class_source
    ).

    TRY.
        " Use XCO Generation APIs to create temporary class
        DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( '$TMP' )->create_put_operation( ).

        " Define class specification
        DATA(lo_class_put) = lo_put_operation->for_clas->add_object( ev_temp_class_name ).
        lo_class_put->set_package( '$TMP' ).
        DATA(lo_specification) = lo_class_put->create_form_specification( ).

        " Set class metadata
        lo_specification->set_short_description( |Temp class for method execution with complex parameters| ).
        
        " Add class source - this makes all types (including local) accessible to RTTI
        lo_specification->definition->add_source( lv_class_source ).

        " Execute the generation
        lo_put_operation->execute( ).

        ev_success = abap_true.
        ev_message = |Temporary execution class { ev_temp_class_name } created successfully.|.

      CATCH cx_root INTO DATA(lx_error).
        ev_message = |XCO class creation failed: { lx_error->get_text( ) }|.
        CLEAR ev_temp_class_name.
    ENDTRY.
  ENDMETHOD.

  METHOD build_executable_class_source.
    " Build complete executable class source
    DATA: lv_types_section TYPE string,
          lv_methods_section TYPE string,
          lv_implementation_section TYPE string.

    " Extract complete class structure
    extract_complete_class_structure(
      EXPORTING iv_class_code = iv_original_class_code
      IMPORTING ev_types_section = lv_types_section
               ev_methods_section = lv_methods_section
               ev_implementation_section = lv_implementation_section
    ).

    " Build complete class definition
    ev_class_source = |CLASS { iv_original_class_name } DEFINITION PUBLIC FINAL CREATE PUBLIC.| &&
                      cl_abap_char_utilities=>newline &&
                      |  PUBLIC SECTION.| &&
                      cl_abap_char_utilities=>newline &&
                      lv_types_section &&
                      lv_methods_section &&
                      |  PROTECTED SECTION.| &&
                      cl_abap_char_utilities=>newline &&
                      |  PRIVATE SECTION.| &&
                      cl_abap_char_utilities=>newline &&
                      |ENDCLASS.| &&
                      cl_abap_char_utilities=>newline &&
                      |CLASS { iv_original_class_name } IMPLEMENTATION.| &&
                      cl_abap_char_utilities=>newline &&
                      lv_implementation_section &&
                      cl_abap_char_utilities=>newline &&
                      |ENDCLASS.|.
  ENDMETHOD.

  METHOD extract_complete_class_structure.
    " Extract types, methods, and implementation from original class
    DATA: lo_string TYPE REF TO if_xco_string,
          lt_lines TYPE string_table,
          lv_in_types TYPE abap_bool,
          lv_in_methods TYPE abap_bool,
          lv_in_implementation TYPE abap_bool,
          lv_current_section TYPE string.

    " Initialize sections
    CLEAR: ev_types_section, ev_methods_section, ev_implementation_section.

    " Use XCO string processing
    lo_string = xco_cp=>string( iv_class_code ).
    lt_lines = lo_string->split( xco_cp_strings=>newline )->value.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lo_line) = xco_cp=>string( lv_line ).
      DATA(lv_line_upper) = lo_line->to_upper_case( )->value.

      " Detect section transitions
      IF lv_line_upper CS 'CLASS' AND lv_line_upper CS 'IMPLEMENTATION'.
        lv_in_implementation = abap_true.
        lv_in_types = abap_false.
        lv_in_methods = abap_false.
        CONTINUE.
      ENDIF.

      IF lv_line_upper CS 'TYPES:' OR ( lv_line_upper CS 'TYPES ' AND lv_line_upper CS 'BEGIN OF' ).
        lv_in_types = abap_true.
        lv_in_methods = abap_false.
        ev_types_section = ev_types_section && lv_line && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      IF lv_line_upper CS 'METHODS:' OR lv_line_upper CS 'METHODS '.
        lv_in_methods = abap_true.
        lv_in_types = abap_false.
        ev_methods_section = ev_methods_section && lv_line && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      " Add lines to appropriate sections
      IF lv_in_types = abap_true.
        ev_types_section = ev_types_section && lv_line && cl_abap_char_utilities=>newline.
        
        " End types section detection
        IF lv_line_upper CS 'METHODS' OR lv_line_upper CS 'DATA:' OR 
           lv_line_upper CS 'PROTECTED SECTION' OR lv_line_upper CS 'PRIVATE SECTION'.
          lv_in_types = abap_false.
        ENDIF.
      ELSEIF lv_in_methods = abap_true.
        ev_methods_section = ev_methods_section && lv_line && cl_abap_char_utilities=>newline.
        
        " End methods section detection
        IF lv_line_upper CS 'PROTECTED SECTION' OR lv_line_upper CS 'PRIVATE SECTION' OR
           lv_line_upper CS 'ENDCLASS'.
          lv_in_methods = abap_false.
        ENDIF.
      ELSEIF lv_in_implementation = abap_true.
        " Skip class implementation header
        IF NOT ( lv_line_upper CS 'CLASS' AND lv_line_upper CS 'IMPLEMENTATION' ).
          ev_implementation_section = ev_implementation_section && lv_line && cl_abap_char_utilities=>newline.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Ensure we have at least basic structure
    IF ev_methods_section IS INITIAL.
      ev_methods_section = |    METHODS: dummy_method.| && cl_abap_char_utilities=>newline.
    ENDIF.

    IF ev_implementation_section IS INITIAL.
      ev_implementation_section = |  METHOD dummy_method.| &&
                                 cl_abap_char_utilities=>newline &&
                                 |    " Placeholder method| &&
                                 cl_abap_char_utilities=>newline &&
                                 |  ENDMETHOD.|.
    ENDIF.
  ENDMETHOD.

  METHOD execute_on_temp_class.
    " Execute method on temporary class (now all types are accessible to RTTI)
    DATA: lt_params TYPE abap_parmbind_tab,
          lv_param_success TYPE abap_bool,
          lv_param_message TYPE string.

    ev_success = abap_false.

    " Build parameter table
    build_parameter_table(
      EXPORTING is_call = is_call
      IMPORTING et_params = lt_params
               ev_success = lv_param_success
               ev_message = lv_param_message
    ).

    IF lv_param_success = abap_false.
      ev_message = lv_param_message.
      RETURN.
    ENDIF.

    TRY.
        DATA lo_obj TYPE REF TO object.
        CREATE OBJECT lo_obj TYPE (iv_temp_class_name).
        CALL METHOD lo_obj->(is_call-method_name)
          PARAMETER-TABLE lt_params.

        " Extract results
        extract_results_from_params(
          EXPORTING is_call = is_call
                   it_params = lt_params
          IMPORTING es_result = ev_result
        ).

        ev_success = abap_true.
        ev_message = 'Method executed successfully using XCO approach.'.

      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_dyn_call_illegal_type
            cx_sy_dyn_call_param_missing
            cx_sy_dyn_call_param_not_found INTO DATA(lo_ex).
        ev_message = lo_ex->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD cleanup_temp_class.
    " Clean up temporary class using XCO
    TRY.
        DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( '$TMP' )->create_delete_operation( ).
        DATA(lo_class_delete) = lo_delete_operation->for_clas->add_object( iv_temp_class_name ).
        lo_delete_operation->execute( ).
        
      CATCH cx_root.
        " Ignore cleanup errors - $TMP objects are temporary anyway
    ENDTRY.
  ENDMETHOD.

  METHOD generate_unique_class_name.
    " Generate unique temporary class name
    DATA(lv_timestamp) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    DATA(lv_random) = cl_abap_random_int=>create( seed = lv_timestamp min = 1000 max = 9999 )->get_next( ).
    ev_class_name = |ZCL_TEMP_{ lv_random }|.
  ENDMETHOD.

  METHOD build_parameter_table.
    " Build parameter table (same logic as original class)
    DATA ls_bind TYPE abap_parmbind.

    ev_success = abap_false.
    CLEAR et_params.

    LOOP AT is_call-params ASSIGNING FIELD-SYMBOL(<lf_param>).
      CLEAR ls_bind.
      ls_bind-name = <lf_param>-name.

      CASE <lf_param>-direction.
        WHEN 'IMPORTING'. ls_bind-kind = cl_abap_objectdescr=>importing.
        WHEN 'EXPORTING'. ls_bind-kind = cl_abap_objectdescr=>exporting.
        WHEN 'CHANGING'. ls_bind-kind = cl_abap_objectdescr=>changing.
        WHEN 'RECEIVING' OR 'RETURNING'. ls_bind-kind = cl_abap_objectdescr=>receiving.
        WHEN OTHERS.
          ev_message = |Invalid parameter direction: { <lf_param>-direction }|.
          RETURN.
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
      INSERT ls_bind INTO TABLE et_params.
    ENDLOOP.

    ev_success = abap_true.
  ENDMETHOD.

  METHOD extract_results_from_params.
    " Extract results from parameter table (same logic as original class)
    DATA param_tab TYPE zmcp_param_tab.

    LOOP AT is_call-params INTO DATA(ls_param) WHERE direction <> 'EXPORTING'.
      READ TABLE it_params INTO DATA(ls_bind_read) WITH KEY name = ls_param-name.
      IF sy-subrc = 0 AND ls_bind_read-value IS BOUND.
        ASSIGN ls_bind_read-value->* TO FIELD-SYMBOL(<ls_out>).
        APPEND VALUE zmcp_param( name      = ls_param-name
                                 direction = ls_param-direction
                                 value     = CONV string( <ls_out> ) )
               TO param_tab.
      ENDIF.
    ENDLOOP.

    es_result-method_name = is_call-method_name.
    es_result-params      = param_tab.
  ENDMETHOD.

  " JSON handling methods (same as original class)
  METHOD is_json_structure_or_table.
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
    
    DATA(lv_trimmed) = |{ iv_json }|.
    CONDENSE lv_trimmed.
    
    IF lv_trimmed CP '[*'.
      parse_json_table( EXPORTING iv_json = iv_json
                        CHANGING  cr_table = <lf_target> ).
    ELSEIF lv_trimmed CP '{*'.
      parse_json_structure( EXPORTING iv_json = iv_json
                            CHANGING  cr_structure = <lf_target> ).
    ELSE.
      <lf_target> = iv_json.
    ENDIF.
  ENDMETHOD.

  METHOD parse_json_structure.
    DATA: lv_json TYPE string,
          lv_field TYPE string,
          lv_value TYPE string.
    
    lv_json = iv_json.
    REPLACE ALL OCCURRENCES OF '{' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '}' IN lv_json WITH ''.
    CONDENSE lv_json.
    
    SPLIT lv_json AT ',' INTO TABLE DATA(lt_pairs).
    
    LOOP AT lt_pairs INTO DATA(lv_pair).
      SPLIT lv_pair AT ':' INTO lv_field lv_value.
      
      REPLACE ALL OCCURRENCES OF '"' IN lv_field WITH ''.
      CONDENSE lv_field.
      
      REPLACE ALL OCCURRENCES OF '"' IN lv_value WITH ''.
      CONDENSE lv_value.
      
      ASSIGN COMPONENT lv_field OF STRUCTURE cr_structure TO FIELD-SYMBOL(<lf_field>).
      IF sy-subrc = 0.
        <lf_field> = lv_value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_json_table.
    DATA: lv_json TYPE string,
          lv_object TYPE string,
          lr_line_ref TYPE REF TO data,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.
    
    lv_json = iv_json.
    REPLACE ALL OCCURRENCES OF '[' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF ']' IN lv_json WITH ''.
    CONDENSE lv_json.
    
    IF lv_json IS INITIAL.
      RETURN.
    ENDIF.
    
    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( cr_table ).
    lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
    
    CLEAR cr_table.
    
    REPLACE ALL OCCURRENCES OF '},{' IN lv_json WITH '}|SEPARATOR|{'.
    SPLIT lv_json AT '|SEPARATOR|' INTO TABLE DATA(lt_objects).
    
    LOOP AT lt_objects INTO lv_object.
      CREATE DATA lr_line_ref TYPE HANDLE lo_struct_descr.
      ASSIGN lr_line_ref->* TO FIELD-SYMBOL(<ls_line>).
      
      parse_json_structure( EXPORTING iv_json = lv_object
                            CHANGING  cr_structure = <ls_line> ).
      
      INSERT <ls_line> INTO TABLE cr_table.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
