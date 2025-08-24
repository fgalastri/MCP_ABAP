CLASS zcl_code_gen_dynamic_handler DEFINITION
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
           END OF ty_parameter_info,
           tt_parameter_info TYPE TABLE OF ty_parameter_info WITH DEFAULT KEY.

    " Structure for method call results
    TYPES: BEGIN OF ty_call_result,
             success TYPE abap_bool,
             message TYPE string,
             generated_program TYPE string,
             execution_time TYPE i,
           END OF ty_call_result.

    " Main method to handle local types dynamically
    METHODS handle_local_types_dynamic
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
               RETURNING VALUE(rs_result) TYPE ty_call_result.

    " Extract local types from class code
    METHODS extract_local_types
               IMPORTING iv_class_code TYPE string
               RETURNING VALUE(rt_types) TYPE string_table.

    " Parse method signature
    METHODS parse_method_signature
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
               RETURNING VALUE(rt_parameters) TYPE tt_parameter_info.

    " Generate wrapper program
    METHODS generate_wrapper_program
               IMPORTING iv_original_class TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
               RETURNING VALUE(rv_program_code) TYPE string.

    " Create parameter assignments from JSON
    METHODS create_parameter_assignments
               IMPORTING it_parameters TYPE tt_parameter_info
                        iv_json_data TYPE string
               RETURNING VALUE(rv_code) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Helper methods
    METHODS find_method_in_code
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
               RETURNING VALUE(rv_method_signature) TYPE string.

    METHODS extract_type_definition
               IMPORTING iv_class_code TYPE string
                        iv_type_name TYPE string
               RETURNING VALUE(rv_definition) TYPE string.

    METHODS parse_json_simple
               IMPORTING iv_json TYPE string
               RETURNING VALUE(rt_fields) TYPE string_table.

    METHODS clean_class_for_program
               IMPORTING iv_class_code TYPE string
               RETURNING VALUE(rv_cleaned_code) TYPE string.
ENDCLASS.

CLASS zcl_code_gen_dynamic_handler IMPLEMENTATION.

  METHOD handle_local_types_dynamic.
    " Main orchestration method
    DATA: lv_start_time TYPE timestampl,
          lv_end_time TYPE timestampl.

    GET TIME STAMP FIELD lv_start_time.
    rs_result-success = abap_false.

    TRY.
        " Generate a complete wrapper program
        DATA(lv_program_code) = generate_wrapper_program(
          iv_original_class = iv_class_code
          iv_method_name = iv_method_name
          iv_parameters_json = iv_parameters_json
        ).

        " Return the generated program
        rs_result-success = abap_true.
        rs_result-message = 'Wrapper program generated successfully'.
        rs_result-generated_program = lv_program_code.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-message = |Error generating program: { lx_error->get_text( ) }|.
    ENDTRY.

    GET TIME STAMP FIELD lv_end_time.
    rs_result-execution_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_start_time
    ).
  ENDMETHOD.

  METHOD extract_local_types.
    " Extract all local type definitions from class code
    DATA: lt_lines TYPE string_table,
          lv_in_types TYPE abap_bool,
          lv_current_type TYPE string,
          lv_brace_count TYPE i.

    SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_line_upper) = to_upper( lv_line ).
      DATA(lv_line_trimmed) = condense( lv_line ).

      " Start of TYPES section
      IF lv_line_upper CS 'TYPES:' OR 
         ( lv_line_upper CS 'TYPES ' AND lv_line_upper CS 'BEGIN OF' ).
        lv_in_types = abap_true.
        lv_current_type = lv_line.
        lv_brace_count = 0.
        
        " Count BEGIN OF occurrences
        IF lv_line_upper CS 'BEGIN OF'.
          lv_brace_count = lv_brace_count + 1.
        ENDIF.
        CONTINUE.
      ENDIF.

      " If we're in types section
      IF lv_in_types = abap_true.
        " Check for end of types section
        IF lv_line_upper CS 'METHODS' OR 
           lv_line_upper CS 'DATA:' OR
           lv_line_upper CS 'CONSTANTS:' OR
           lv_line_upper CS 'CLASS-METHODS' OR
           lv_line_upper CS 'EVENTS:' OR
           lv_line_upper CS 'PROTECTED SECTION' OR
           lv_line_upper CS 'PRIVATE SECTION'.
          
          " Save current type if exists
          IF lv_current_type IS NOT INITIAL.
            APPEND lv_current_type TO rt_types.
            CLEAR lv_current_type.
          ENDIF.
          lv_in_types = abap_false.
          CONTINUE.
        ENDIF.

        " Continue building type definition
        lv_current_type = lv_current_type && cl_abap_char_utilities=>newline && lv_line.

        " Track BEGIN OF / END OF pairs
        IF lv_line_upper CS 'BEGIN OF'.
          lv_brace_count = lv_brace_count + 1.
        ELSEIF lv_line_upper CS 'END OF'.
          lv_brace_count = lv_brace_count - 1.
        ENDIF.

        " If we've closed all braces and hit a period, type is complete
        IF lv_brace_count = 0 AND lv_line CS '.'.
          APPEND lv_current_type TO rt_types.
          CLEAR lv_current_type.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Add last type if exists
    IF lv_current_type IS NOT INITIAL.
      APPEND lv_current_type TO rt_types.
    ENDIF.
  ENDMETHOD.

  METHOD parse_method_signature.
    " Parse method signature to extract parameter information
    DATA(lv_signature) = find_method_in_code(
      iv_class_code = iv_class_code
      iv_method_name = iv_method_name
    ).

    " Simple parsing of method signature
    DATA: lt_parts TYPE string_table,
          lv_current_kind TYPE string.

    " Split signature into parts
    REPLACE ALL OCCURRENCES OF 'IMPORTING' IN lv_signature WITH '|IMPORTING'.
    REPLACE ALL OCCURRENCES OF 'EXPORTING' IN lv_signature WITH '|EXPORTING'.
    REPLACE ALL OCCURRENCES OF 'CHANGING' IN lv_signature WITH '|CHANGING'.
    REPLACE ALL OCCURRENCES OF 'RETURNING' IN lv_signature WITH '|RETURNING'.

    SPLIT lv_signature AT '|' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO DATA(lv_part).
      DATA(lv_part_clean) = condense( lv_part ).
      
      " Skip empty parts and method declaration
      IF lv_part_clean IS INITIAL OR lv_part_clean CS 'METHODS'.
        CONTINUE.
      ENDIF.

      " Determine parameter kind
      IF lv_part_clean CS 'IMPORTING'.
        lv_current_kind = 'IMPORTING'.
        REPLACE 'IMPORTING' IN lv_part_clean WITH ''.
      ELSEIF lv_part_clean CS 'EXPORTING'.
        lv_current_kind = 'EXPORTING'.
        REPLACE 'EXPORTING' IN lv_part_clean WITH ''.
      ELSEIF lv_part_clean CS 'CHANGING'.
        lv_current_kind = 'CHANGING'.
        REPLACE 'CHANGING' IN lv_part_clean WITH ''.
      ELSEIF lv_part_clean CS 'RETURNING'.
        lv_current_kind = 'RETURNING'.
        REPLACE 'RETURNING' IN lv_part_clean WITH ''.
      ENDIF.

      " Parse parameter definitions
      IF lv_part_clean CS 'TYPE'.
        SPLIT lv_part_clean AT ' TYPE ' INTO DATA(lv_param_name) DATA(lv_param_type).
        
        " Clean parameter name
        lv_param_name = condense( lv_param_name ).
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN lv_param_name WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN lv_param_name WITH ''.
        
        " Clean parameter type
        lv_param_type = condense( lv_param_type ).
        REPLACE ALL OCCURRENCES OF '.' IN lv_param_type WITH ''.
        REPLACE ALL OCCURRENCES OF ',' IN lv_param_type WITH ''.

        " Check if it's a local type
        DATA(lv_is_local) = COND abap_bool(
          WHEN lv_param_type CS 'TY_' OR lv_param_type CS 'TT_' OR lv_param_type CS 'TS_'
          THEN abap_true
          ELSE abap_false
        ).

        " Extract type definition if local
        DATA(lv_type_def) = COND string(
          WHEN lv_is_local = abap_true
          THEN extract_type_definition( iv_class_code = iv_class_code
                                       iv_type_name = lv_param_type )
          ELSE ''
        ).

        " Add parameter info
        APPEND VALUE ty_parameter_info(
          name = lv_param_name
          kind = lv_current_kind
          type_name = lv_param_type
          is_local_type = lv_is_local
          type_definition = lv_type_def
        ) TO rt_parameters.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD generate_wrapper_program.
    " Generate a complete ABAP program that can handle local types
    DATA: lt_local_types TYPE string_table,
          lv_types_section TYPE string,
          lv_class_section TYPE string,
          lv_execution_section TYPE string.

    " Extract local types
    lt_local_types = extract_local_types( iv_original_class ).

    " Build types section
    LOOP AT lt_local_types INTO DATA(lv_type).
      lv_types_section = lv_types_section && lv_type && cl_abap_char_utilities=>newline.
    ENDLOOP.

    " Clean and prepare class code
    lv_class_section = clean_class_for_program( iv_original_class ).

    " Parse method parameters
    DATA(lt_parameters) = parse_method_signature(
      iv_class_code = iv_original_class
      iv_method_name = iv_method_name
    ).

    " Generate parameter assignments
    DATA(lv_param_assignments) = create_parameter_assignments(
      it_parameters = lt_parameters
      iv_json_data = iv_parameters_json
    ).

    " Build execution section
    lv_execution_section = |START-OF-SELECTION.| &&
                          cl_abap_char_utilities=>newline &&
                          lv_param_assignments &&
                          cl_abap_char_utilities=>newline &&
                          |  DATA(lo_instance) = NEW lcl_generated_class( ).| &&
                          cl_abap_char_utilities=>newline &&
                          |  TRY.| &&
                          cl_abap_char_utilities=>newline &&
                          |    lo_instance->{ iv_method_name }( ).| &&
                          cl_abap_char_utilities=>newline &&
                          |    WRITE: 'Method executed successfully'.| &&
                          cl_abap_char_utilities=>newline &&
                          |  CATCH cx_root INTO DATA(lx_error).| &&
                          cl_abap_char_utilities=>newline &&
                          |    WRITE: 'Error:', lx_error->get_text( ).| &&
                          cl_abap_char_utilities=>newline &&
                          |  ENDTRY.|.

    " Combine all sections
    rv_program_code = |REPORT zdynamic_local_type_caller.| &&
                     cl_abap_char_utilities=>newline &&
                     cl_abap_char_utilities=>newline &&
                     |" Local types extracted from original class| &&
                     cl_abap_char_utilities=>newline &&
                     lv_types_section &&
                     cl_abap_char_utilities=>newline &&
                     |" Modified class for local execution| &&
                     cl_abap_char_utilities=>newline &&
                     lv_class_section &&
                     cl_abap_char_utilities=>newline &&
                     |" Execution logic| &&
                     cl_abap_char_utilities=>newline &&
                     lv_execution_section.
  ENDMETHOD.

  METHOD create_parameter_assignments.
    " Generate ABAP code for parameter assignments from JSON
    DATA: lt_json_fields TYPE string_table.

    lt_json_fields = parse_json_simple( iv_json_data ).

    LOOP AT it_parameters INTO DATA(ls_param).
      " Find corresponding JSON value
      DATA(lv_json_value) = ''.
      LOOP AT lt_json_fields INTO DATA(lv_field).
        IF lv_field CS ls_param-name.
          SPLIT lv_field AT ':' INTO DATA(lv_name) DATA(lv_value).
          lv_json_value = condense( lv_value ).
          EXIT.
        ENDIF.
      ENDLOOP.

      " Generate assignment code
      rv_code = rv_code &&
               |  DATA: { ls_param-name } TYPE { ls_param-type_name }.| &&
               cl_abap_char_utilities=>newline.

      " Add value assignment if JSON value found
      IF lv_json_value IS NOT INITIAL.
        rv_code = rv_code &&
                 |  { ls_param-name } = '{ lv_json_value }'.| &&
                 cl_abap_char_utilities=>newline.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_method_in_code.
    " Find method signature in class code
    DATA: lt_lines TYPE string_table,
          lv_in_method TYPE abap_bool,
          lv_method_pattern TYPE string.

    lv_method_pattern = |METHODS { iv_method_name }|.
    SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_line_upper) = to_upper( lv_line ).

      " Check if this line starts the method definition
      IF lv_line_upper CS to_upper( lv_method_pattern ).
        lv_in_method = abap_true.
        rv_method_signature = lv_line.
        CONTINUE.
      ENDIF.

      " Continue collecting method signature
      IF lv_in_method = abap_true.
        rv_method_signature = rv_method_signature && | { lv_line }|.
        
        " End of method definition
        IF lv_line CS '.' OR lv_line_upper CS 'METHODS '.
          IF lv_line CS '.'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_type_definition.
    " Extract specific type definition from class code
    DATA: lt_lines TYPE string_table,
          lv_in_type TYPE abap_bool,
          lv_brace_count TYPE i.

    SPLIT iv_class_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      " Check if this line contains our type definition
      IF lv_line CS iv_type_name AND lv_line CS 'BEGIN OF'.
        lv_in_type = abap_true.
        lv_brace_count = 1.
        rv_definition = lv_line.
        CONTINUE.
      ENDIF.

      " Continue collecting type definition
      IF lv_in_type = abap_true.
        rv_definition = rv_definition && cl_abap_char_utilities=>newline && lv_line.

        " Track nested structures
        IF lv_line CS 'BEGIN OF'.
          lv_brace_count = lv_brace_count + 1.
        ELSEIF lv_line CS 'END OF'.
          lv_brace_count = lv_brace_count - 1.
          
          " If we've closed all braces, type definition is complete
          IF lv_brace_count = 0.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_json_simple.
    " Simple JSON parsing
    DATA: lv_json TYPE string.

    lv_json = iv_json.
    
    " Remove braces and quotes
    REPLACE ALL OCCURRENCES OF '{' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '}' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN lv_json WITH ''.
    
    SPLIT lv_json AT ',' INTO TABLE rt_fields.
  ENDMETHOD.

  METHOD clean_class_for_program.
    " Clean class code for inclusion in program
    rv_cleaned_code = iv_class_code.
    
    " Convert to local class
    REPLACE 'PUBLIC' IN rv_cleaned_code WITH ''.
    REPLACE 'FINAL' IN rv_cleaned_code WITH ''.
    REPLACE 'CREATE PUBLIC' IN rv_cleaned_code WITH ''.
    
    " Rename class to local format
    REPLACE ALL OCCURRENCES OF REGEX 'CLASS\s+(\w+)\s+DEFINITION'
            IN rv_cleaned_code WITH 'CLASS lcl_generated_class DEFINITION'.
    
    REPLACE ALL OCCURRENCES OF REGEX 'CLASS\s+(\w+)\s+IMPLEMENTATION'
            IN rv_cleaned_code WITH 'CLASS lcl_generated_class IMPLEMENTATION'.
  ENDMETHOD.

ENDCLASS.
