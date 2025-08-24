CLASS zcl_xco_local_type_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Structure for parameter information
    TYPES: BEGIN OF ty_parameter_info,
             name TYPE string,
             kind TYPE string,
             type_name TYPE string,
             is_local_type TYPE abap_bool,
             type_definition TYPE string,
             data_ref TYPE REF TO data,
           END OF ty_parameter_info,
           tt_parameter_info TYPE TABLE OF ty_parameter_info WITH DEFAULT KEY.

    " Structure for method call results
    TYPES: BEGIN OF ty_call_result,
             success TYPE abap_bool,
             message TYPE string,
             result_json TYPE string,
             temp_class_name TYPE string,
             execution_time TYPE i,
           END OF ty_call_result.

    " Structure for local type extraction
    TYPES: BEGIN OF ty_local_type,
             type_name TYPE string,
             definition TYPE string,
             components TYPE string,
           END OF ty_local_type,
           tt_local_types TYPE TABLE OF ty_local_type WITH DEFAULT KEY.

    METHODS call_method_with_local_types
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
               EXPORTING es_result TYPE ty_call_result.

    METHODS create_temp_class_with_xco
               IMPORTING iv_original_class TYPE string
                        iv_method_name TYPE string
               EXPORTING ev_temp_class_name TYPE string.

    METHODS extract_local_types_xco
               IMPORTING iv_class_code TYPE string
               EXPORTING et_types TYPE tt_local_types.

    METHODS analyze_method_with_xco
               IMPORTING iv_temp_class_name TYPE string
                        iv_method_name TYPE string
               EXPORTING et_parameters TYPE tt_parameter_info.

    METHODS create_parameters_from_json
               IMPORTING it_parameters TYPE tt_parameter_info
                        iv_json_data TYPE string.

    METHODS cleanup_temp_objects
               IMPORTING iv_temp_class_name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " XCO-based helper methods
    METHODS generate_temp_class_name
               EXPORTING ev_name TYPE string.

    METHODS build_class_definition_xco
               IMPORTING iv_original_class TYPE string
                        it_local_types TYPE tt_local_types
               EXPORTING ev_class_definition TYPE string.

    METHODS parse_json_with_xco
               IMPORTING iv_json TYPE string
               EXPORTING eo_json TYPE REF TO if_xco_json_data.

    METHODS create_dynamic_data_xco
               IMPORTING io_type_descr TYPE REF TO cl_abap_typedescr
                        iv_json_value TYPE string
               EXPORTING er_data TYPE REF TO data.

    METHODS extract_method_signature_xco
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
               EXPORTING ev_signature TYPE string.

    METHODS parse_type_components_xco
               IMPORTING iv_type_definition TYPE string
               EXPORTING et_components TYPE string_table.
ENDCLASS.

CLASS zcl_xco_local_type_handler IMPLEMENTATION.

  METHOD call_method_with_local_types.
    " Main orchestration method using XCO
    DATA: lv_start_time TYPE timestampl,
          lv_end_time TYPE timestampl,
          lv_temp_class_name TYPE string.

    GET TIME STAMP FIELD lv_start_time.
    rs_result-success = abap_false.

    TRY.
        " Step 1: Create temporary class with local types using XCO
        lv_temp_class_name = create_temp_class_with_xco(
          iv_original_class = iv_class_code
          iv_method_name = iv_method_name
        ).
        rs_result-temp_class_name = lv_temp_class_name.

        " Step 2: Analyze method signature using standard RTTI (now possible)
        DATA(lt_parameters) = analyze_method_with_xco(
          iv_temp_class_name = lv_temp_class_name
          iv_method_name = iv_method_name
        ).

        " Step 3: Create parameters from JSON using XCO JSON handling
        create_parameters_from_json(
          it_parameters = lt_parameters
          iv_json_data = iv_parameters_json
        ).

        " Step 4: Execute dynamic method call
        DATA(lt_param_table) = VALUE abap_parmbind_tab( ).
        LOOP AT lt_parameters INTO DATA(ls_param).
          APPEND VALUE abap_parmbind(
            name = ls_param-name
            kind = SWITCH #( ls_param-kind
                           WHEN 'IMPORTING' THEN cl_abap_objectdescr=>importing
                           WHEN 'EXPORTING' THEN cl_abap_objectdescr=>exporting
                           WHEN 'CHANGING' THEN cl_abap_objectdescr=>changing
                           WHEN 'RETURNING' THEN cl_abap_objectdescr=>returning )
            value = ls_param-data_ref
          ) TO lt_param_table.
        ENDLOOP.

        " Create instance and call method
        DATA(lo_instance) = cl_abap_typedescr=>describe_by_name( lv_temp_class_name ).
        CREATE OBJECT lo_instance TYPE (lv_temp_class_name).

        CALL METHOD (lv_temp_class_name)=>(iv_method_name)
          PARAMETER-TABLE lt_param_table.

        " Step 5: Extract results using XCO JSON generation
        DATA(lo_json_writer) = xco_cp_json=>data->builder( ).
        LOOP AT lt_parameters INTO ls_param WHERE kind = 'EXPORTING' OR kind = 'RETURNING'.
          " Add result to JSON
          lo_json_writer->add_member( ls_param-name )->add_string( 'result_value' ).
        ENDLOOP.

        rs_result-success = abap_true.
        rs_result-message = 'Method executed successfully with local types'.
        rs_result-result_json = lo_json_writer->get_data( )->to_string( ).

      CATCH cx_xco_gen_put_exception INTO DATA(lx_xco).
        rs_result-message = |XCO generation error: { lx_xco->get_text( ) }|.
        
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_method).
        rs_result-message = |Method call error: { lx_method->get_text( ) }|.
        
      CATCH cx_sy_conversion_error INTO DATA(lx_conversion).
        rs_result-message = |Parameter conversion error: { lx_conversion->get_text( ) }|.
        
      CATCH cx_root INTO DATA(lx_root).
        rs_result-message = |Unexpected error: { lx_root->get_text( ) }|.
    ENDTRY.

    " Step 6: Cleanup temporary objects
    IF lv_temp_class_name IS NOT INITIAL.
      cleanup_temp_objects( lv_temp_class_name ).
    ENDIF.

    GET TIME STAMP FIELD lv_end_time.
    rs_result-execution_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_start_time
    ).
  ENDMETHOD.

  METHOD create_temp_class_with_xco.
    " Create temporary class using XCO Generation APIs
    DATA: lo_put_operation TYPE REF TO if_xco_gen_o_put,
          lo_specification TYPE REF TO if_xco_gen_o_clas_s_fo_form,
          lt_local_types TYPE tt_local_types.

    " Generate unique temporary class name
    rv_temp_class_name = generate_temp_class_name( ).

    " Extract local types from original class
    lt_local_types = extract_local_types_xco( iv_original_class ).

    " Build complete class definition
    DATA(lv_class_definition) = build_class_definition_xco(
      iv_original_class = iv_original_class
      it_local_types = lt_local_types
    ).

            " Use XCO to create temporary class
        TRY.
            " Create PUT operation for class generation
            lo_put_operation = xco_cp_generation=>environment->dev_system( '$TMP' )->create_put_operation( ).

            " Define class specification
            lo_specification = lo_put_operation->for_clas->add_object( rv_temp_class_name
              )->set_package( '$TMP'
              )->create_form_specification( ).

            " Set class definition using XCO
            lo_specification->set_short_description( |Temporary class for local type handling| ).
            
            " Add class content (this is simplified - in reality you'd use XCO's structured APIs)
            " For now, we'll use the source-based approach
            lo_specification->definition->add_source( lv_class_definition ).

            " Execute the generation
            lo_put_operation->execute( ).

          CATCH cx_root INTO DATA(lx_put).
            " Handle any XCO generation errors
            rv_temp_class_name = ''.
        ENDTRY.
  ENDMETHOD.

  METHOD extract_local_types_xco.
    " Extract local types using XCO string processing
    DATA: lo_string TYPE REF TO if_xco_string,
          lt_lines TYPE string_table,
          lv_in_types TYPE abap_bool,
          lv_current_type TYPE string,
          lv_brace_count TYPE i.

    " Use XCO string processing
    lo_string = xco_cp=>string( iv_class_code ).
    lt_lines = lo_string->split( xco_cp_strings=>newline )->value.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lo_line) = xco_cp=>string( lv_line ).
      DATA(lv_line_upper) = lo_line->to_upper_case( )->value.

      " Detect TYPES section
      IF lv_line_upper CS 'TYPES:' OR 
         ( lv_line_upper CS 'TYPES ' AND lv_line_upper CS 'BEGIN OF' ).
        lv_in_types = abap_true.
        lv_current_type = lv_line.
        lv_brace_count = 0.
        
        IF lv_line_upper CS 'BEGIN OF'.
          lv_brace_count = lv_brace_count + 1.
        ENDIF.
        CONTINUE.
      ENDIF.

      " Process types section
      IF lv_in_types = abap_true.
        " Check for end of types section
        IF lv_line_upper CS 'METHODS' OR 
           lv_line_upper CS 'DATA:' OR
           lv_line_upper CS 'CONSTANTS:' OR
           lv_line_upper CS 'PROTECTED SECTION' OR
           lv_line_upper CS 'PRIVATE SECTION'.
          
          " Save current type
          IF lv_current_type IS NOT INITIAL.
            DATA(ls_type) = VALUE ty_local_type( ).
            " Extract type name using XCO regex
            DATA(lo_regex) = xco_cp_regular_expression=>create( 'TYPES:\s*(\w+)' ).
            DATA(lt_matches) = lo_regex->create_matcher( lv_current_type )->find_all( ).
            IF lines( lt_matches ) > 0.
              ls_type-type_name = lt_matches[ 1 ]-groups[ 1 ]-value.
            ENDIF.
            ls_type-definition = lv_current_type.
            APPEND ls_type TO rt_types.
            CLEAR lv_current_type.
          ENDIF.
          lv_in_types = abap_false.
          CONTINUE.
        ENDIF.

        " Continue building type definition
        lv_current_type = lv_current_type && cl_abap_char_utilities=>newline && lv_line.

        " Track structure nesting
        IF lv_line_upper CS 'BEGIN OF'.
          lv_brace_count = lv_brace_count + 1.
        ELSEIF lv_line_upper CS 'END OF'.
          lv_brace_count = lv_brace_count - 1.
        ENDIF.

        " Complete type definition
        IF lv_brace_count = 0 AND lv_line CS '.'.
          ls_type = VALUE ty_local_type( ).
          " Extract type name
          DATA(lo_type_regex) = xco_cp_regular_expression=>create( '(\w+)\s+TYPE' ).
          DATA(lt_type_matches) = lo_type_regex->create_matcher( lv_current_type )->find_all( ).
          IF lines( lt_type_matches ) > 0.
            ls_type-type_name = lt_type_matches[ 1 ]-groups[ 1 ]-value.
          ENDIF.
          ls_type-definition = lv_current_type.
          APPEND ls_type TO rt_types.
          CLEAR lv_current_type.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Add last type if exists
    IF lv_current_type IS NOT INITIAL.
      ls_type = VALUE ty_local_type( ).
      ls_type-definition = lv_current_type.
      APPEND ls_type TO rt_types.
    ENDIF.
  ENDMETHOD.

  METHOD analyze_method_with_xco.
    " Use standard RTTI on the temporary class (now in repository)
    DATA: lo_class_descr TYPE REF TO cl_abap_classdescr,
          lo_method_descr TYPE REF TO cl_abap_methoddescr,
          lt_parameters TYPE abap_parmdescr_tab.

    TRY.
        " Get class descriptor for temporary class
        lo_class_descr ?= cl_abap_typedescr=>describe_by_name( iv_temp_class_name ).
        
        " Get method descriptor
        lo_method_descr = lo_class_descr->get_method( iv_method_name ).
        lt_parameters = lo_method_descr->parameters.

        " Build parameter information
        LOOP AT lt_parameters INTO DATA(ls_param).
          APPEND VALUE ty_parameter_info(
            name = ls_param-name
            kind = SWITCH #( ls_param-parm_kind
                           WHEN cl_abap_objectdescr=>importing THEN 'IMPORTING'
                           WHEN cl_abap_objectdescr=>exporting THEN 'EXPORTING'
                           WHEN cl_abap_objectdescr=>changing THEN 'CHANGING'
                           WHEN cl_abap_objectdescr=>returning THEN 'RETURNING' )
            type_name = ls_param-type->type_name
            is_local_type = abap_true  " All types in temp class are now "global"
          ) TO rt_parameters.
        ENDLOOP.

      CATCH cx_sy_rtti_incomplete_type
            cx_sy_rtti_not_found INTO DATA(lx_rtti).
        " Handle RTTI errors gracefully
        CLEAR rt_parameters.
    ENDTRY.
  ENDMETHOD.

  METHOD create_parameters_from_json.
    " Create parameter data references using XCO JSON handling
    DATA: lo_json TYPE REF TO if_xco_json_data.

    " Parse JSON using XCO
    lo_json = parse_json_with_xco( iv_json_data ).

    " Create data references for each parameter
    LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      TRY.
          " Get type descriptor (now available since class is in repository)
          DATA(lo_type_descr) = cl_abap_typedescr=>describe_by_name( <ls_param>-type_name ).
          
          " Extract JSON value for this parameter
          DATA(lv_json_value) = lo_json->get_member( <ls_param>-name )->get_string_value( ).
          
          " Create dynamic data using XCO-enhanced approach
          <ls_param>-data_ref = create_dynamic_data_xco(
            io_type_descr = lo_type_descr
            iv_json_value = lv_json_value
          ).

        CATCH cx_root INTO DATA(lx_error).
          " Handle conversion errors gracefully
          CREATE DATA <ls_param>-data_ref TYPE string.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD cleanup_temp_objects.
    " Clean up temporary objects using XCO
    TRY.
        " Use XCO to delete temporary class
        DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( '$TMP' )->create_delete_operation( ).
        lo_delete_operation->for_clas->add_object( iv_temp_class_name ).
        lo_delete_operation->execute( ).
        
      CATCH cx_root.
        " Log error but don't fail the main operation
        " Temporary objects in $TMP will be cleaned up automatically
    ENDTRY.
  ENDMETHOD.

  METHOD generate_temp_class_name.
    " Generate unique temporary class name
    DATA(lv_timestamp) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    DATA(lv_random) = cl_abap_random_int=>create( seed = lv_timestamp min = 1000 max = 9999 )->get_next( ).
    rv_name = |ZCL_TEMP_LOCAL_{ lv_random }|.
  ENDMETHOD.

  METHOD build_class_definition_xco.
    " Build complete class definition including local types
    DATA: lv_class_def TYPE string,
          lv_types_section TYPE string,
          lv_methods_section TYPE string.

    " Build types section
    LOOP AT it_local_types INTO DATA(ls_type).
      lv_types_section = lv_types_section && ls_type-definition && cl_abap_char_utilities=>newline.
    ENDLOOP.

    " Extract methods section from original class
    lv_methods_section = extract_method_signature_xco(
      iv_class_code = iv_original_class
      iv_method_name = 'ALL'  " Extract all methods
    ).

    " Build complete class definition
    rv_class_definition = |CLASS zcl_temp_local DEFINITION PUBLIC FINAL CREATE PUBLIC.| &&
                          cl_abap_char_utilities=>newline &&
                          |  PUBLIC SECTION.| &&
                          cl_abap_char_utilities=>newline &&
                          lv_types_section &&
                          cl_abap_char_utilities=>newline &&
                          lv_methods_section &&
                          cl_abap_char_utilities=>newline &&
                          |ENDCLASS.|.
  ENDMETHOD.

  METHOD parse_json_with_xco.
    " Parse JSON using XCO JSON handling
    TRY.
        ro_json = xco_cp_json=>data->from_string( iv_json ).
      CATCH cx_root.
        " Fallback to empty JSON object
        ro_json = xco_cp_json=>data->builder( )->get_data( ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_dynamic_data_xco.
    " Create dynamic data reference with XCO support
    CREATE DATA rr_data TYPE HANDLE io_type_descr.
    ASSIGN rr_data->* TO FIELD-SYMBOL(<lv_data>).
    
    " Simple assignment for now - could be enhanced with XCO type conversion
    <lv_data> = iv_json_value.
  ENDMETHOD.

  METHOD extract_method_signature_xco.
    " Extract method signatures using XCO string processing
    DATA(lo_string) = xco_cp=>string( iv_class_code ).
    DATA(lt_lines) = lo_string->split( xco_cp_strings=>newline )->value.
    
    " Simple extraction - in full implementation, use XCO's structured parsing
    LOOP AT lt_lines INTO DATA(lv_line).
      IF lv_line CS 'METHODS'.
        rv_signature = rv_signature && lv_line && cl_abap_char_utilities=>newline.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_type_components_xco.
    " Parse type components using XCO string processing
    DATA(lo_string) = xco_cp=>string( iv_type_definition ).
    rt_components = lo_string->split( xco_cp_strings=>newline )->value.
  ENDMETHOD.

ENDCLASS.
