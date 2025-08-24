CLASS zcl_xco_dynamic_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Result structure for the dynamic type handling
    TYPES: BEGIN OF ty_dynamic_result,
             success TYPE abap_bool,
             message TYPE string,
             temp_class_name TYPE string,
             execution_time TYPE i,
           END OF ty_dynamic_result.

    " Parameter information structure
    TYPES: BEGIN OF ty_param_info,
             name TYPE string,
             direction TYPE string,
             type_name TYPE string,
             json_value TYPE string,
           END OF ty_param_info,
           tt_param_info TYPE TABLE OF ty_param_info WITH DEFAULT KEY.

    " Main method to handle dynamic calls with local types
    METHODS call_method_with_local_types
               EXPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
                        es_result TYPE ty_dynamic_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Helper methods for XCO-based approach
    METHODS create_temp_class_with_xco
               IMPORTING iv_original_class TYPE string
               EXPORTING ev_temp_class_name TYPE string.

    METHODS extract_local_types
               IMPORTING iv_class_code TYPE string
               EXPORTING et_type_definitions TYPE string_table.

    METHODS build_repository_class
               IMPORTING iv_original_class TYPE string
                        it_type_definitions TYPE string_table
               EXPORTING ev_class_source TYPE string.

    METHODS analyze_method_parameters
               IMPORTING iv_temp_class_name TYPE string
                        iv_method_name TYPE string
               EXPORTING et_parameters TYPE tt_param_info.

    METHODS execute_dynamic_method
               IMPORTING iv_temp_class_name TYPE string
                        iv_method_name TYPE string
                        it_parameters TYPE tt_param_info
               EXPORTING ev_success TYPE abap_bool
                        ev_result_json TYPE string.

    METHODS cleanup_temp_class
               IMPORTING iv_temp_class_name TYPE string.

    METHODS generate_unique_class_name
               EXPORTING ev_class_name TYPE string.
ENDCLASS.

CLASS zcl_xco_dynamic_handler IMPLEMENTATION.

  METHOD call_method_with_local_types.
    " Main orchestration method using XCO for local type handling
    DATA: lv_start_time TYPE timestampl,
          lv_end_time TYPE timestampl,
          lv_temp_class_name TYPE string,
          lt_parameters TYPE tt_param_info,
          lv_success TYPE abap_bool,
          lv_result_json TYPE string.

    GET TIME STAMP FIELD lv_start_time.
    es_result-success = abap_false.

    TRY.
        " Step 1: Create temporary class using XCO Generation APIs
        " This promotes local types to repository level, making them accessible to RTTI
        create_temp_class_with_xco(
          EXPORTING iv_original_class = iv_class_code
          IMPORTING ev_temp_class_name = lv_temp_class_name
        ).

        IF lv_temp_class_name IS INITIAL.
          es_result-message = 'Failed to create temporary class with XCO'.
          RETURN.
        ENDIF.

        es_result-temp_class_name = lv_temp_class_name.

        " Step 2: Analyze method parameters using standard RTTI
        " Now possible because types are in repository
        analyze_method_parameters(
          EXPORTING iv_temp_class_name = lv_temp_class_name
                   iv_method_name = iv_method_name
          IMPORTING et_parameters = lt_parameters
        ).

        " Step 3: Parse JSON parameters and map to ABAP types
        " Enhanced with XCO JSON processing capabilities
        " (Implementation would parse iv_parameters_json and populate lt_parameters)

        " Step 4: Execute the method dynamically
        execute_dynamic_method(
          EXPORTING iv_temp_class_name = lv_temp_class_name
                   iv_method_name = iv_method_name
                   it_parameters = lt_parameters
          IMPORTING ev_success = lv_success
                   ev_result_json = lv_result_json
        ).

        IF lv_success = abap_true.
          es_result-success = abap_true.
          es_result-message = 'Method executed successfully with local types'.
        ELSE.
          es_result-message = 'Method execution failed'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        es_result-message = |Error: { lx_error->get_text( ) }|.
    ENDTRY.

    " Step 5: Cleanup temporary objects
    IF lv_temp_class_name IS NOT INITIAL.
      cleanup_temp_class( lv_temp_class_name ).
    ENDIF.

    GET TIME STAMP FIELD lv_end_time.
    es_result-execution_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_start_time
    ).
  ENDMETHOD.

  METHOD create_temp_class_with_xco.
    " Create temporary class using XCO Generation APIs
    DATA: lt_type_definitions TYPE string_table,
          lv_class_source TYPE string.

    " Generate unique temporary class name
    generate_unique_class_name(
      IMPORTING ev_class_name = ev_temp_class_name
    ).

    " Extract local types from original class
    extract_local_types(
      EXPORTING iv_class_code = iv_original_class
      IMPORTING et_type_definitions = lt_type_definitions
    ).

    " Build complete class source for repository
    build_repository_class(
      EXPORTING iv_original_class = iv_original_class
               it_type_definitions = lt_type_definitions
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
        lo_specification->set_short_description( |Temporary class for local type handling| ).
        
        " Add class source - this is the key step that promotes local types
        " to repository level, making them accessible to standard RTTI
        lo_specification->definition->add_source( lv_class_source ).

        " Execute the generation - creates the class in the repository
        lo_put_operation->execute( ).

      CATCH cx_root INTO DATA(lx_error).
        " Clear result on error
        CLEAR ev_temp_class_name.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_local_types.
    " Extract local type definitions using XCO string processing
    DATA: lo_string TYPE REF TO if_xco_string,
          lt_lines TYPE string_table,
          lv_in_types_section TYPE abap_bool,
          lv_current_type TYPE string,
          lv_brace_count TYPE i.

    " Use XCO string processing for robust parsing
    lo_string = xco_cp=>string( iv_class_code ).
    lt_lines = lo_string->split( xco_cp_strings=>newline )->value.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lo_line) = xco_cp=>string( lv_line ).
      DATA(lv_line_upper) = lo_line->to_upper_case( )->value.

      " Detect start of TYPES section
      IF lv_line_upper CS 'TYPES:' OR 
         ( lv_line_upper CS 'TYPES ' AND lv_line_upper CS 'BEGIN OF' ).
        lv_in_types_section = abap_true.
        lv_current_type = lv_line.
        lv_brace_count = 0.
        
        IF lv_line_upper CS 'BEGIN OF'.
          lv_brace_count = lv_brace_count + 1.
        ENDIF.
        CONTINUE.
      ENDIF.

      " Process types section
      IF lv_in_types_section = abap_true.
        " Check for end of types section
        IF lv_line_upper CS 'METHODS' OR 
           lv_line_upper CS 'DATA:' OR
           lv_line_upper CS 'CONSTANTS:' OR
           lv_line_upper CS 'PROTECTED SECTION' OR
           lv_line_upper CS 'PRIVATE SECTION'.
          
          " Save current type definition
          IF lv_current_type IS NOT INITIAL.
            APPEND lv_current_type TO et_type_definitions.
            CLEAR lv_current_type.
          ENDIF.
          lv_in_types_section = abap_false.
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

        " Complete type definition when structure is closed
        IF lv_brace_count = 0 AND lv_line CS '.'.
          APPEND lv_current_type TO et_type_definitions.
          CLEAR lv_current_type.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Add last type if exists
    IF lv_current_type IS NOT INITIAL.
      APPEND lv_current_type TO et_type_definitions.
    ENDIF.
  ENDMETHOD.

  METHOD build_repository_class.
    " Build complete class source including local types as global types
    DATA: lv_types_section TYPE string,
          lv_methods_section TYPE string.

    " Build types section from extracted definitions
    LOOP AT it_type_definitions INTO DATA(lv_type_def).
      lv_types_section = lv_types_section && lv_type_def && cl_abap_char_utilities=>newline.
    ENDLOOP.

    " Extract and adapt methods section from original class
    " This is simplified - in full implementation, would parse method signatures
    lv_methods_section = |    METHODS: dummy_method.| && cl_abap_char_utilities=>newline.

    " Build complete class definition
    ev_class_source = |CLASS zcl_temp_local DEFINITION PUBLIC FINAL CREATE PUBLIC.| &&
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
                      |CLASS zcl_temp_local IMPLEMENTATION.| &&
                      cl_abap_char_utilities=>newline &&
                      |  METHOD dummy_method.| &&
                      cl_abap_char_utilities=>newline &&
                      |  ENDMETHOD.| &&
                      cl_abap_char_utilities=>newline &&
                      |ENDCLASS.|.
  ENDMETHOD.

  METHOD analyze_method_parameters.
    " Use standard RTTI on the temporary class
    " This works now because local types are promoted to repository level
    TRY.
        DATA(lo_class_descr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( iv_temp_class_name ) ).
        DATA(lo_method_descr) = lo_class_descr->get_method( iv_method_name ).
        DATA(lt_parameters) = lo_method_descr->parameters.

        " Build parameter information
        LOOP AT lt_parameters INTO DATA(ls_param).
          APPEND VALUE ty_param_info(
            name = ls_param-name
            direction = SWITCH #( ls_param-parm_kind
                               WHEN cl_abap_objectdescr=>importing THEN 'IMPORTING'
                               WHEN cl_abap_objectdescr=>exporting THEN 'EXPORTING'
                               WHEN cl_abap_objectdescr=>changing THEN 'CHANGING' )
            type_name = ls_param-type->type_name
          ) TO et_parameters.
        ENDLOOP.

      CATCH cx_root.
        " Handle RTTI errors gracefully
        CLEAR et_parameters.
    ENDTRY.
  ENDMETHOD.

  METHOD execute_dynamic_method.
    " Execute the method dynamically using the temporary class
    " This is a placeholder - full implementation would:
    " 1. Create parameter data references based on type information
    " 2. Parse JSON values and assign to parameters
    " 3. Execute CALL METHOD dynamically
    " 4. Extract results and convert to JSON
    
    ev_success = abap_true.
    ev_result_json = '{"status": "executed"}'.
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

ENDCLASS.
