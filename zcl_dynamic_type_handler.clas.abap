CLASS zcl_dynamic_type_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Main structure for parameter information
    TYPES: BEGIN OF ty_parameter_info,
             name TYPE string,
             kind TYPE abap_parmkind,
             type_kind TYPE abap_typekind,
             type_name TYPE string,
             is_local_type TYPE abap_bool,
             data_ref TYPE REF TO data,
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

    METHODS analyze_method_signature
               IMPORTING iv_class_name TYPE string
                        iv_method_name TYPE string
               RETURNING VALUE(rt_parameters) TYPE tt_parameter_info
               RAISING cx_sy_dyn_call_illegal_method.
               
    METHODS create_dynamic_parameter
               IMPORTING iv_parameter_info TYPE ty_parameter_info
                        iv_json_data TYPE string
               RETURNING VALUE(rr_data_ref) TYPE REF TO data
               RAISING cx_sy_conversion_error.

    METHODS call_method_with_local_types
               IMPORTING iv_class_name TYPE string
                        iv_method_name TYPE string
                        iv_parameters_json TYPE string
               RETURNING VALUE(rs_result) TYPE ty_call_result.

    METHODS parse_json_to_fields
               IMPORTING iv_json TYPE string
               RETURNING VALUE(rt_fields) TYPE tt_json_fields.

    METHODS convert_value_by_type
               IMPORTING iv_value TYPE string
                        io_type_descr TYPE REF TO cl_abap_typedescr
               RETURNING VALUE(rv_converted) TYPE REF TO data
               RAISING cx_sy_conversion_error.

    METHODS get_type_descriptor
               IMPORTING iv_class_name TYPE string
                        iv_method_name TYPE string
                        iv_parameter_name TYPE string
               RETURNING VALUE(ro_type_descr) TYPE REF TO cl_abap_typedescr
               RAISING cx_sy_dyn_call_illegal_method.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS is_local_type
               IMPORTING io_type_descr TYPE REF TO cl_abap_typedescr
               RETURNING VALUE(rv_is_local) TYPE abap_bool.

    METHODS create_structure_from_json
               IMPORTING io_struct_descr TYPE REF TO cl_abap_structdescr
                        iv_json_data TYPE string
               RETURNING VALUE(rr_structure) TYPE REF TO data
               RAISING cx_sy_conversion_error.

    METHODS create_table_from_json
               IMPORTING io_table_descr TYPE REF TO cl_abap_tabledescr
                        iv_json_data TYPE string
               RETURNING VALUE(rr_table) TYPE REF TO data
               RAISING cx_sy_conversion_error.

    METHODS assign_field_value
               IMPORTING ir_field_ref TYPE REF TO data
                        iv_value TYPE string
                        io_type_descr TYPE REF TO cl_abap_typedescr
               RAISING cx_sy_conversion_error.

    METHODS extract_json_value
               IMPORTING iv_json TYPE string
                        iv_field_name TYPE string
               RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

CLASS zcl_dynamic_type_handler IMPLEMENTATION.

  METHOD analyze_method_signature.
    DATA: lo_class_descr TYPE REF TO cl_abap_classdescr,
          lo_method_descr TYPE REF TO cl_abap_methoddescr,
          lt_parameters TYPE abap_parmdescr_tab.

    TRY.
        " Get class descriptor
        lo_class_descr ?= cl_abap_typedescr=>describe_by_name( iv_class_name ).
        
        " Get method descriptor
        lo_method_descr = lo_class_descr->get_method( iv_method_name ).
        lt_parameters = lo_method_descr->parameters.

        " Analyze each parameter
        LOOP AT lt_parameters INTO DATA(ls_param).
          DATA(ls_param_info) = VALUE ty_parameter_info(
            name = ls_param-name
            kind = ls_param-parm_kind
            type_kind = ls_param-type_kind
            type_name = ls_param-type->type_name
            is_local_type = is_local_type( ls_param-type )
          ).
          
          APPEND ls_param_info TO rt_parameters.
        ENDLOOP.

      CATCH cx_sy_rtti_incomplete_type
            cx_sy_rtti_not_found INTO DATA(lx_rtti).
        RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_method
          EXPORTING
            previous = lx_rtti.
    ENDTRY.
  ENDMETHOD.

  METHOD create_dynamic_parameter.
    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr,
          lo_elem_descr TYPE REF TO cl_abap_elemdescr.

    TRY.
        " Get type descriptor from method signature
        DATA(lo_type_descr) = get_type_descriptor(
          iv_class_name = 'LCL_' && iv_parameter_info-type_name
          iv_method_name = 'GET_TYPE_INFO'  " Placeholder
          iv_parameter_name = iv_parameter_info-name
        ).

        CASE iv_parameter_info-type_kind.
          WHEN cl_abap_typedescr=>typekind_struct1 OR
               cl_abap_typedescr=>typekind_struct2.
            " Handle structure types
            lo_struct_descr ?= lo_type_descr.
            rr_data_ref = create_structure_from_json(
              io_struct_descr = lo_struct_descr
              iv_json_data = iv_json_data
            ).

          WHEN cl_abap_typedescr=>typekind_table.
            " Handle table types
            lo_table_descr ?= lo_type_descr.
            rr_data_ref = create_table_from_json(
              io_table_descr = lo_table_descr
              iv_json_data = iv_json_data
            ).

          WHEN OTHERS.
            " Handle elementary types
            lo_elem_descr ?= lo_type_descr.
            rr_data_ref = convert_value_by_type(
              iv_value = iv_json_data
              io_type_descr = lo_elem_descr
            ).
        ENDCASE.

      CATCH cx_sy_move_cast_error
            cx_sy_rtti_incomplete_type INTO DATA(lx_error).
        RAISE EXCEPTION TYPE cx_sy_conversion_error
          EXPORTING
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD call_method_with_local_types.
    DATA: lt_param_info TYPE tt_parameter_info,
          lt_param_table TYPE abap_parmbind_tab,
          lv_start_time TYPE timestampl,
          lv_end_time TYPE timestampl.

    " Initialize result
    rs_result-success = abap_false.
    GET TIME STAMP FIELD lv_start_time.

    TRY.
        " Analyze method signature
        lt_param_info = analyze_method_signature(
          iv_class_name = iv_class_name
          iv_method_name = iv_method_name
        ).

        " Parse JSON parameters
        DATA(lt_json_params) = parse_json_to_fields( iv_parameters_json ).

        " Create dynamic parameters
        LOOP AT lt_param_info INTO DATA(ls_param_info).
          DATA(ls_param_bind) = VALUE abap_parmbind(
            name = ls_param_info-name
            kind = ls_param_info-kind
          ).

          " Find corresponding JSON data
          READ TABLE lt_json_params WITH KEY field_name = ls_param_info-name
               INTO DATA(ls_json_param).
          
          IF sy-subrc = 0.
            " Create parameter with JSON data
            ls_param_bind-value = create_dynamic_parameter(
              iv_parameter_info = ls_param_info
              iv_json_data = ls_json_param-field_value
            ).
          ELSE.
            " Create empty parameter
            ls_param_bind-value = create_dynamic_parameter(
              iv_parameter_info = ls_param_info
              iv_json_data = '{}'
            ).
          ENDIF.

          APPEND ls_param_bind TO lt_param_table.
        ENDLOOP.

        " Execute dynamic method call
        CALL METHOD (iv_class_name)=>(iv_method_name)
          PARAMETER-TABLE lt_param_table.

        " Extract results from EXPORTING/CHANGING/RETURNING parameters
        " (This would need enhancement based on specific requirements)
        rs_result-success = abap_true.
        rs_result-message = 'Method executed successfully'.
        rs_result-result_json = '{"status": "success"}'. " Placeholder

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lx_method).
        rs_result-message = |Method call failed: { lx_method->get_text( ) }|.
        
      CATCH cx_sy_conversion_error INTO DATA(lx_conversion).
        rs_result-message = |Parameter conversion failed: { lx_conversion->get_text( ) }|.
        
      CATCH cx_root INTO DATA(lx_root).
        rs_result-message = |Unexpected error: { lx_root->get_text( ) }|.
    ENDTRY.

    " Calculate execution time
    GET TIME STAMP FIELD lv_end_time.
    rs_result-execution_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = lv_start_time
    ).
  ENDMETHOD.

  METHOD parse_json_to_fields.
    " Simplified JSON parsing - in real implementation, use proper JSON parser
    DATA: lv_json TYPE string,
          lt_lines TYPE string_table.

    lv_json = iv_json.
    
    " Remove braces and split by commas (simplified approach)
    REPLACE ALL OCCURRENCES OF '{' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '}' IN lv_json WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN lv_json WITH ''.
    
    SPLIT lv_json AT ',' INTO TABLE lt_lines.
    
    LOOP AT lt_lines INTO DATA(lv_line).
      SPLIT lv_line AT ':' INTO DATA(lv_field_name) DATA(lv_field_value).
      
      IF lv_field_name IS NOT INITIAL AND lv_field_value IS NOT INITIAL.
        APPEND VALUE ty_json_field(
          field_name = |{ lv_field_name }|
          field_value = |{ lv_field_value }|
          field_type = 'STRING'  " Default type
        ) TO rt_fields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_value_by_type.
    DATA: lr_data TYPE REF TO data.

    CASE io_type_descr->type_kind.
      WHEN cl_abap_typedescr=>typekind_char.
        CREATE DATA lr_data TYPE c LENGTH 255.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_char>).
        <lv_char> = iv_value.
        
      WHEN cl_abap_typedescr=>typekind_string.
        CREATE DATA lr_data TYPE string.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_string>).
        <lv_string> = iv_value.
        
      WHEN cl_abap_typedescr=>typekind_int.
        CREATE DATA lr_data TYPE i.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_int>).
        <lv_int> = iv_value.
        
      WHEN cl_abap_typedescr=>typekind_packed.
        CREATE DATA lr_data TYPE p LENGTH 16 DECIMALS 2.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_packed>).
        <lv_packed> = iv_value.
        
      WHEN cl_abap_typedescr=>typekind_date.
        CREATE DATA lr_data TYPE dats.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_date>).
        <lv_date> = iv_value.
        
      WHEN cl_abap_typedescr=>typekind_time.
        CREATE DATA lr_data TYPE tims.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_time>).
        <lv_time> = iv_value.
        
      WHEN OTHERS.
        " Default to string
        CREATE DATA lr_data TYPE string.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_default>).
        <lv_default> = iv_value.
    ENDCASE.
    
    rv_converted = lr_data.
  ENDMETHOD.

  METHOD get_type_descriptor.
    " This is a placeholder - in real implementation, this would use
    " more sophisticated RTTI to get the actual type descriptor
    " from the method signature
    
    " For now, return a basic string descriptor
    ro_type_descr = cl_abap_typedescr=>describe_by_name( 'STRING' ).
  ENDMETHOD.

  METHOD is_local_type.
    " Check if type name contains local class indicators
    DATA(lv_type_name) = io_type_descr->type_name.
    
    " Local types typically have specific naming patterns
    IF lv_type_name CS 'LCL_' OR
       lv_type_name CS '%_' OR
       lv_type_name CS 'TY_' OR
       strlen( lv_type_name ) = 0.
      rv_is_local = abap_true.
    ELSE.
      rv_is_local = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD create_structure_from_json.
    DATA: lt_components TYPE cl_abap_structdescr=>component_table,
          lr_structure TYPE REF TO data.

    " Get structure components
    lt_components = io_struct_descr->get_components( ).
    
    " Create dynamic structure
    CREATE DATA lr_structure TYPE HANDLE io_struct_descr.
    ASSIGN lr_structure->* TO FIELD-SYMBOL(<ls_structure>).
    
    " Parse JSON and assign values to structure fields
    DATA(lt_json_fields) = parse_json_to_fields( iv_json_data ).
    
    LOOP AT lt_components INTO DATA(ls_component).
      " Find corresponding JSON field
      READ TABLE lt_json_fields WITH KEY field_name = ls_component-name
           INTO DATA(ls_json_field).
           
      IF sy-subrc = 0.
        " Assign field value
        ASSIGN COMPONENT ls_component-name OF STRUCTURE <ls_structure>
               TO FIELD-SYMBOL(<lv_field>).
        IF sy-subrc = 0.
          TRY.
              assign_field_value(
                ir_field_ref = REF #( <lv_field> )
                iv_value = ls_json_field-field_value
                io_type_descr = ls_component-as_include
              ).
            CATCH cx_sy_conversion_error.
              " Handle conversion errors gracefully
              CLEAR <lv_field>.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDLOOP.
    
    rr_structure = lr_structure.
  ENDMETHOD.

  METHOD create_table_from_json.
    " Placeholder for table creation from JSON
    " In real implementation, this would parse JSON array
    " and create table entries
    
    CREATE DATA rr_table TYPE HANDLE io_table_descr.
  ENDMETHOD.

  METHOD assign_field_value.
    ASSIGN ir_field_ref->* TO FIELD-SYMBOL(<lv_target_field>).
    
    CASE io_type_descr->type_kind.
      WHEN cl_abap_typedescr=>typekind_char OR
           cl_abap_typedescr=>typekind_string.
        <lv_target_field> = iv_value.
        
      WHEN cl_abap_typedescr=>typekind_int.
        <lv_target_field> = CONV i( iv_value ).
        
      WHEN cl_abap_typedescr=>typekind_packed.
        <lv_target_field> = CONV p( iv_value ).
        
      WHEN cl_abap_typedescr=>typekind_date.
        <lv_target_field> = CONV dats( iv_value ).
        
      WHEN cl_abap_typedescr=>typekind_time.
        <lv_target_field> = CONV tims( iv_value ).
        
      WHEN OTHERS.
        <lv_target_field> = iv_value.
    ENDCASE.
  ENDMETHOD.

  METHOD extract_json_value.
    " Simplified JSON value extraction
    DATA: lv_pattern TYPE string,
          lv_json TYPE string.
    
    lv_json = iv_json.
    lv_pattern = |"{ iv_field_name }":"([^"]*)"|.
    
    FIND REGEX lv_pattern IN lv_json SUBMATCHES rv_value.
    
    IF sy-subrc <> 0.
      " Try without quotes
      lv_pattern = |"{ iv_field_name }":([^,}]*)|.
      FIND REGEX lv_pattern IN lv_json SUBMATCHES rv_value.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
