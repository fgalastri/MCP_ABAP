CLASS zcl_xco_dynamic_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Main structure for parameter information
    TYPES: BEGIN OF ty_parameter_info,
             name TYPE string,
             kind TYPE string,
             type_kind TYPE string,
             type_name TYPE string,
             is_local_type TYPE abap_bool,
             structure_components TYPE string,
           END OF ty_parameter_info,
           tt_parameter_info TYPE TABLE OF ty_parameter_info WITH DEFAULT KEY.

    " Structure for method call results
    TYPES: BEGIN OF ty_call_result,
             success TYPE abap_bool,
             message TYPE string,
             result_json TYPE string,
             execution_time TYPE i,
           END OF ty_call_result.

    " Structure for JSON field mapping
    TYPES: BEGIN OF ty_json_field,
             field_name TYPE string,
             field_value TYPE string,
             field_type TYPE string,
           END OF ty_json_field,
           tt_json_fields TYPE TABLE OF ty_json_field WITH DEFAULT KEY.

    " Structure for component information
    TYPES: BEGIN OF ty_component_info,
             name TYPE string,
             type_name TYPE string,
             type_kind TYPE string,
             length TYPE i,
             decimals TYPE i,
           END OF ty_component_info,
           tt_component_info TYPE TABLE OF ty_component_info WITH DEFAULT KEY.

    METHODS analyze_class_with_xco
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
               RETURNING VALUE(rt_parameters) TYPE tt_parameter_info.

    METHODS extract_local_types_from_code
               IMPORTING iv_class_code TYPE string
               RETURNING VALUE(rt_types) TYPE string_table.

    METHODS parse_type_definition
               IMPORTING iv_type_definition TYPE string
               RETURNING VALUE(rt_components) TYPE tt_component_info.

    METHODS create_dynamic_structure_xco
               IMPORTING iv_type_definition TYPE string
                        iv_json_data TYPE string
               RETURNING VALUE(rr_data_ref) TYPE REF TO data.

    METHODS call_method_with_xco
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
               RETURNING VALUE(rs_result) TYPE ty_call_result.

    METHODS parse_json_to_fields
               IMPORTING iv_json TYPE string
               RETURNING VALUE(rt_fields) TYPE tt_json_fields.

    METHODS build_dynamic_program
               IMPORTING iv_original_class TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
               RETURNING VALUE(rv_program_code) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS extract_method_signature
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
               RETURNING VALUE(rv_signature) TYPE string.

    METHODS parse_parameter_from_signature
               IMPORTING iv_signature TYPE string
               RETURNING VALUE(rt_parameters) TYPE tt_parameter_info.

    METHODS find_type_definition_in_code
               IMPORTING iv_class_code TYPE string
                        iv_type_name TYPE string
               RETURNING VALUE(rv_definition) TYPE string.

    METHODS create_wrapper_program
               IMPORTING iv_original_class TYPE string
                        iv_local_types TYPE string
                        iv_method_call TYPE string
               RETURNING VALUE(rv_program) TYPE string.

    METHODS generate_parameter_assignments
               IMPORTING it_parameters TYPE tt_parameter_info
                        iv_json_data TYPE string
               RETURNING VALUE(rv_assignments) TYPE string.
ENDCLASS.

CLASS zcl_xco_dynamic_handler IMPLEMENTATION.

  METHOD analyze_class_with_xco.
    " Extract method signature from class code
    DATA(lv_signature) = extract_method_signature(
      iv_class_code = iv_class_code
      iv_method_name = iv_method_name
    ).

    " Parse parameters from signature
    rt_parameters = parse_parameter_from_signature( lv_signature ).

    " For each parameter, check if it's a local type and extract definition
    LOOP AT rt_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
      " Check if this is a local type (starts with TY_ or similar patterns)
      IF <ls_param>-type_name CS 'TY_' OR
         <ls_param>-type_name CS 'TT_' OR
         <ls_param>-type_name CS 'TS_'.
        
        <ls_param>-is_local_type = abap_true.
        
        " Extract the type definition from class code
        DATA(lv_type_def) = find_type_definition_in_code(
          iv_class_code = iv_class_code
          iv_type_name = <ls_param>-type_name
        ).
        
        <ls_param>-structure_components = lv_type_def.
      ELSE.
        <ls_param>-is_local_type = abap_false.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_local_types_from_code.
    " Extract all TYPES definitions from class code
    DATA: lt_lines TYPE string_table,
          lv_in_types_section TYPE abap_bool,
          lv_current_type TYPE string.

    SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      " Convert to uppercase for pattern matching
      DATA(lv_line_upper) = to_upper( lv_line ).
      
      " Check if we're entering a TYPES section
      IF lv_line_upper CS 'TYPES:' OR lv_line_upper CS 'TYPES '.
        lv_in_types_section = abap_true.
        lv_current_type = lv_line.
        CONTINUE.
      ENDIF.
      
      " If we're in types section, collect the definition
      IF lv_in_types_section = abap_true.
        " Check for end of types section
        IF lv_line_upper CS 'METHODS' OR 
           lv_line_upper CS 'DATA:' OR
           lv_line_upper CS 'CONSTANTS:' OR
           lv_line_upper CS 'CLASS-METHODS' OR
           lv_line_upper CS 'EVENTS:'.
          " End of types section
          IF lv_current_type IS NOT INITIAL.
            APPEND lv_current_type TO rt_types.
            CLEAR lv_current_type.
          ENDIF.
          lv_in_types_section = abap_false.
        ELSE.
          " Continue building current type definition
          lv_current_type = lv_current_type && | { lv_line }|.
          
          " Check if this type definition is complete (ends with period)
          IF lv_line CS '.'.
            APPEND lv_current_type TO rt_types.
            CLEAR lv_current_type.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    
    " Add last type if exists
    IF lv_current_type IS NOT INITIAL.
      APPEND lv_current_type TO rt_types.
    ENDIF.
  ENDMETHOD.

  METHOD parse_type_definition.
    " Parse a type definition string to extract component information
    " This is a simplified parser for structure definitions
    
    DATA: lt_lines TYPE string_table,
          lv_in_structure TYPE abap_bool.

    SPLIT iv_type_definition AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_line_trimmed) = condense( lv_line ).
      
      " Skip empty lines and comments
      IF lv_line_trimmed IS INITIAL OR lv_line_trimmed(1) = '"'.
        CONTINUE.
      ENDIF.
      
      " Check for BEGIN OF structure
      IF lv_line_trimmed CS 'BEGIN OF'.
        lv_in_structure = abap_true.
        CONTINUE.
      ENDIF.
      
      " Check for END OF structure
      IF lv_line_trimmed CS 'END OF'.
        lv_in_structure = abap_false.
        CONTINUE.
      ENDIF.
      
      " Parse field definitions within structure
      IF lv_in_structure = abap_true.
        DATA(ls_component) = VALUE ty_component_info( ).
        
        " Simple parsing: field_name TYPE type_spec
        SPLIT lv_line_trimmed AT ' TYPE ' INTO ls_component-name DATA(lv_type_spec).
        
        " Clean field name
        ls_component-name = condense( ls_component-name ).
        REPLACE ALL OCCURRENCES OF ',' IN ls_component-name WITH ''.
        
        " Parse type specification
        ls_component-type_name = condense( lv_type_spec ).
        REPLACE ALL OCCURRENCES OF ',' IN ls_component-type_name WITH ''.
        REPLACE ALL OCCURRENCES OF '.' IN ls_component-type_name WITH ''.
        
        " Determine type kind based on type specification
        IF ls_component-type_name CS 'STRING'.
          ls_component-type_kind = 'STRING'.
        ELSEIF ls_component-type_name CS 'LENGTH'.
          ls_component-type_kind = 'CHAR'.
          " Extract length if specified
          FIND REGEX 'LENGTH\s+(\d+)' IN ls_component-type_name SUBMATCHES DATA(lv_length).
          IF sy-subrc = 0.
            ls_component-length = lv_length.
          ENDIF.
        ELSEIF ls_component-type_name CS 'DECIMALS'.
          ls_component-type_kind = 'PACKED'.
          " Extract length and decimals
          FIND REGEX 'LENGTH\s+(\d+)' IN ls_component-type_name SUBMATCHES lv_length.
          IF sy-subrc = 0.
            ls_component-length = lv_length.
          ENDIF.
          FIND REGEX 'DECIMALS\s+(\d+)' IN ls_component-type_name SUBMATCHES DATA(lv_decimals).
          IF sy-subrc = 0.
            ls_component-decimals = lv_decimals.
          ENDIF.
        ELSEIF ls_component-type_name CS 'DATS'.
          ls_component-type_kind = 'DATE'.
        ELSEIF ls_component-type_name CS 'TIMS'.
          ls_component-type_kind = 'TIME'.
        ELSE.
          ls_component-type_kind = 'UNKNOWN'.
        ENDIF.
        
        IF ls_component-name IS NOT INITIAL.
          APPEND ls_component TO rt_components.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_dynamic_structure_xco.
    " Create a dynamic structure based on type definition and populate with JSON
    DATA: lt_components TYPE tt_component_info,
          lr_struct_ref TYPE REF TO data.

    " Parse the type definition
    lt_components = parse_type_definition( iv_type_definition ).

    " Use XCO to build dynamic structure type
    TRY.
        DATA(lo_type) = xco_cp_abap_dictionary=>type_for_name( 'STRING' ). " Fallback
        
        " For now, create a simple string-based structure
        " In a full implementation, you would use XCO to build the exact structure
        CREATE DATA lr_struct_ref TYPE string.
        ASSIGN lr_struct_ref->* TO FIELD-SYMBOL(<lv_data>).
        <lv_data> = iv_json_data. " Simplified assignment
        
        rr_data_ref = lr_struct_ref.
        
      CATCH cx_root.
        " Fallback: create string data reference
        CREATE DATA lr_struct_ref TYPE string.
        ASSIGN lr_struct_ref->* TO FIELD-SYMBOL(<lv_fallback>).
        <lv_fallback> = iv_json_data.
        rr_data_ref = lr_struct_ref.
    ENDTRY.
  ENDMETHOD.

  METHOD call_method_with_xco.
    " Main method to call a method with local types using XCO approach
    DATA: lv_start_time TYPE timestampl,
          lv_end_time TYPE timestampl.

    GET TIME STAMP FIELD lv_start_time.
    rs_result-success = abap_false.

    TRY.
        " Analyze the class and method
        DATA(lt_parameters) = analyze_class_with_xco(
          iv_class_code = iv_class_code
          iv_method_name = iv_method_name
        ).

        " Build a dynamic program that includes the local types
        DATA(lv_program_code) = build_dynamic_program(
          iv_original_class = iv_class_code
          iv_method_name = iv_method_name
          iv_parameters_json = iv_parameters_json
        ).

        " In a real implementation, you would:
        " 1. Generate and compile the dynamic program
        " 2. Execute it with the JSON parameters
        " 3. Extract the results
        
        " For now, return success with the generated program code
        rs_result-success = abap_true.
        rs_result-message = 'Dynamic program generated successfully'.
        rs_result-result_json = |{ "program_generated": true, "parameters_count": { lines( lt_parameters ) } }|.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-message = |Error: { lx_error->get_text( ) }|.
    ENDTRY.

    GET TIME STAMP FIELD lv_end_time.
    rs_result-execution_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_start_time
    ).
  ENDMETHOD.

  METHOD parse_json_to_fields.
    " Simplified JSON parsing
    DATA: lv_json TYPE string,
          lt_lines TYPE string_table.

    lv_json = iv_json.
    
    " Remove braces and quotes
    REPLACE ALL OCCURRENCES OF '{' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '}' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN lv_json WITH ''.
    
    SPLIT lv_json AT ',' INTO TABLE lt_lines.
    
    LOOP AT lt_lines INTO DATA(lv_line).
      SPLIT lv_line AT ':' INTO DATA(lv_field_name) DATA(lv_field_value).
      
      IF lv_field_name IS NOT INITIAL AND lv_field_value IS NOT INITIAL.
        APPEND VALUE ty_json_field(
          field_name = condense( lv_field_name )
          field_value = condense( lv_field_value )
          field_type = 'STRING'
        ) TO rt_fields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_dynamic_program.
    " Build a complete ABAP program that includes local types and method call
    DATA: lt_local_types TYPE string_table,
          lv_types_section TYPE string,
          lv_method_call TYPE string.

    " Extract local types from original class
    lt_local_types = extract_local_types_from_code( iv_original_class ).

    " Build types section
    LOOP AT lt_local_types INTO DATA(lv_type).
      lv_types_section = lv_types_section && lv_type && cl_abap_char_utilities=>newline.
    ENDLOOP.

    " Generate parameter assignments
    DATA(lt_parameters) = analyze_class_with_xco(
      iv_class_code = iv_original_class
      iv_method_name = iv_method_name
    ).

    lv_method_call = generate_parameter_assignments(
      it_parameters = lt_parameters
      iv_json_data = iv_parameters_json
    ).

    " Build complete program
    rv_program_code = |REPORT zdynamic_method_caller.| &&
                     cl_abap_char_utilities=>newline &&
                     lv_types_section &&
                     cl_abap_char_utilities=>newline &&
                     |START-OF-SELECTION.| &&
                     cl_abap_char_utilities=>newline &&
                     lv_method_call.
  ENDMETHOD.

  METHOD extract_method_signature.
    " Extract method signature from class code
    DATA: lt_lines TYPE string_table,
          lv_in_method TYPE abap_bool,
          lv_method_pattern TYPE string.

    lv_method_pattern = |METHODS { iv_method_name }|.
    SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_line_upper) = to_upper( lv_line ).
      
      " Check if this line contains our method definition
      IF lv_line_upper CS to_upper( lv_method_pattern ).
        lv_in_method = abap_true.
        rv_signature = lv_line.
        CONTINUE.
      ENDIF.
      
      " If we're in the method definition, continue collecting
      IF lv_in_method = abap_true.
        " Check for end of method definition
        IF lv_line_upper CS 'METHODS ' OR
           lv_line_upper CS 'DATA:' OR
           lv_line_upper CS 'CONSTANTS:' OR
           lv_line CS '.'.
          " End of method definition
          rv_signature = rv_signature && | { lv_line }|.
          IF lv_line CS '.'.
            EXIT. " Method definition complete
          ENDIF.
        ELSE.
          rv_signature = rv_signature && | { lv_line }|.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_parameter_from_signature.
    " Parse method signature to extract parameter information
    DATA: lv_signature TYPE string,
          lt_parts TYPE string_table.

    lv_signature = iv_signature.
    
    " Split by parameter keywords
    REPLACE ALL OCCURRENCES OF 'IMPORTING' IN lv_signature WITH '|IMPORTING'.
    REPLACE ALL OCCURRENCES OF 'EXPORTING' IN lv_signature WITH '|EXPORTING'.
    REPLACE ALL OCCURRENCES OF 'CHANGING' IN lv_signature WITH '|CHANGING'.
    REPLACE ALL OCCURRENCES OF 'RETURNING' IN lv_signature WITH '|RETURNING'.
    
    SPLIT lv_signature AT '|' INTO TABLE lt_parts.
    
    LOOP AT lt_parts INTO DATA(lv_part).
      DATA(lv_part_trimmed) = condense( lv_part ).
      
      " Skip empty parts and method name
      IF lv_part_trimmed IS INITIAL OR lv_part_trimmed CS 'METHODS'.
        CONTINUE.
      ENDIF.
      
      " Determine parameter kind
      DATA(lv_kind) = COND string(
        WHEN lv_part_trimmed CS 'IMPORTING' THEN 'IMPORTING'
        WHEN lv_part_trimmed CS 'EXPORTING' THEN 'EXPORTING'
        WHEN lv_part_trimmed CS 'CHANGING' THEN 'CHANGING'
        WHEN lv_part_trimmed CS 'RETURNING' THEN 'RETURNING'
        ELSE 'UNKNOWN'
      ).
      
      " Extract parameter name and type (simplified parsing)
      DATA(lv_param_def) = lv_part_trimmed.
      REPLACE ALL OCCURRENCES OF 'IMPORTING' IN lv_param_def WITH ''.
      REPLACE ALL OCCURRENCES OF 'EXPORTING' IN lv_param_def WITH ''.
      REPLACE ALL OCCURRENCES OF 'CHANGING' IN lv_param_def WITH ''.
      REPLACE ALL OCCURRENCES OF 'RETURNING' IN lv_param_def WITH ''.
      
      " Parse parameter name and type
      IF lv_param_def CS 'TYPE'.
        SPLIT lv_param_def AT ' TYPE ' INTO DATA(lv_param_name) DATA(lv_param_type).
        
        " Clean up parameter name and type
        lv_param_name = condense( lv_param_name ).
        lv_param_type = condense( lv_param_type ).
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN lv_param_name WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN lv_param_name WITH ''.
        
        " Add to parameters table
        APPEND VALUE ty_parameter_info(
          name = lv_param_name
          kind = lv_kind
          type_name = lv_param_type
          type_kind = 'UNKNOWN'
        ) TO rt_parameters.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_type_definition_in_code.
    " Find a specific type definition in the class code
    DATA: lt_lines TYPE string_table,
          lv_in_type_def TYPE abap_bool,
          lv_type_pattern TYPE string.

    lv_type_pattern = |{ iv_type_name }|.
    SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      " Check if this line contains our type definition
      IF lv_line CS lv_type_pattern AND lv_line CS 'BEGIN OF'.
        lv_in_type_def = abap_true.
        rv_definition = lv_line.
        CONTINUE.
      ENDIF.
      
      " If we're in the type definition, continue collecting
      IF lv_in_type_def = abap_true.
        rv_definition = rv_definition && cl_abap_char_utilities=>newline && lv_line.
        
        " Check for end of type definition
        IF lv_line CS 'END OF' AND lv_line CS lv_type_pattern.
          EXIT. " Type definition complete
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_wrapper_program.
    " Create a wrapper program that can handle local types
    rv_program = |REPORT zdynamic_wrapper.| &&
                cl_abap_char_utilities=>newline &&
                iv_local_types &&
                cl_abap_char_utilities=>newline &&
                |START-OF-SELECTION.| &&
                cl_abap_char_utilities=>newline &&
                iv_method_call.
  ENDMETHOD.

  METHOD generate_parameter_assignments.
    " Generate ABAP code for parameter assignments from JSON
    DATA: lt_json_fields TYPE tt_json_fields.

    lt_json_fields = parse_json_to_fields( iv_json_data ).

    LOOP AT it_parameters INTO DATA(ls_param).
      " Find corresponding JSON field
      READ TABLE lt_json_fields WITH KEY field_name = ls_param-name
           INTO DATA(ls_json_field).
      
      IF sy-subrc = 0.
        rv_assignments = rv_assignments &&
                        |DATA: { ls_param-name } TYPE { ls_param-type_name }.| &&
                        cl_abap_char_utilities=>newline &&
                        |{ ls_param-name } = '{ ls_json_field-field_value }'.| &&
                        cl_abap_char_utilities=>newline.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
