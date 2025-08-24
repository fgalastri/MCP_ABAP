CLASS zcl_xco_simple_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Simplified structure for demonstration
    TYPES: BEGIN OF ty_call_result,
             success TYPE abap_bool,
             message TYPE string,
             temp_class_name TYPE string,
           END OF ty_call_result.

    " Main method to demonstrate XCO approach
    METHODS handle_local_types_with_xco
               IMPORTING iv_class_code TYPE string
                        iv_method_name TYPE string
               EXPORTING es_result TYPE ty_call_result.

    " Helper methods
    METHODS create_temp_class_xco
               IMPORTING iv_class_code TYPE string
               EXPORTING ev_temp_class_name TYPE string.

    METHODS extract_types_with_xco
               IMPORTING iv_class_code TYPE string
               EXPORTING et_type_names TYPE string_table.

    METHODS cleanup_temp_class
               IMPORTING iv_temp_class_name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS generate_unique_name
               EXPORTING ev_class_name TYPE string.

    METHODS build_temp_class_source
               IMPORTING iv_original_code TYPE string
                        it_type_names TYPE string_table
               EXPORTING ev_class_source TYPE string.
ENDCLASS.

CLASS zcl_xco_simple_handler IMPLEMENTATION.

  METHOD handle_local_types_with_xco.
    " Main orchestration method demonstrating XCO approach
    DATA: lv_temp_class_name TYPE string,
          lt_type_names TYPE string_table.

    es_result-success = abap_false.

    TRY.
        " Step 1: Extract local types using XCO string processing
        extract_types_with_xco(
          EXPORTING iv_class_code = iv_class_code
          IMPORTING et_type_names = lt_type_names
        ).

        " Step 2: Create temporary class with XCO Generation APIs
        create_temp_class_xco(
          EXPORTING iv_class_code = iv_class_code
          IMPORTING ev_temp_class_name = lv_temp_class_name
        ).

        IF lv_temp_class_name IS NOT INITIAL.
          es_result-success = abap_true.
          es_result-message = |Temporary class { lv_temp_class_name } created successfully|.
          es_result-temp_class_name = lv_temp_class_name.

          " Step 3: Now standard RTTI can be used on the temporary class
          " because local types are now "global" in the repository
          
          " Step 4: Cleanup
          cleanup_temp_class( lv_temp_class_name ).
        ELSE.
          es_result-message = 'Failed to create temporary class'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        es_result-message = |Error: { lx_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD create_temp_class_xco.
    " Create temporary class using XCO Generation APIs
    DATA: lo_put_operation TYPE REF TO if_xco_gen_o_put,
          lo_specification TYPE REF TO if_xco_gen_o_clas_s_fo_form,
          lv_class_source TYPE string,
          lt_type_names TYPE string_table.

    " Generate unique temporary class name
    generate_unique_name(
      IMPORTING ev_class_name = ev_temp_class_name
    ).

    " Extract type information
    extract_types_with_xco(
      EXPORTING iv_class_code = iv_class_code
      IMPORTING et_type_names = lt_type_names
    ).

    " Build complete class source
    build_temp_class_source(
      EXPORTING iv_original_code = iv_class_code
                it_type_names = lt_type_names
      IMPORTING ev_class_source = lv_class_source
    ).

    TRY.
        " Create PUT operation for class generation
        lo_put_operation = xco_cp_generation=>environment->dev_system( '$TMP' )->create_put_operation( ).

        " Define class specification
        lo_specification = lo_put_operation->for_clas->add_object( ev_temp_class_name
          )->set_package( '$TMP'
          )->create_form_specification( ).

        " Set class definition using XCO
        lo_specification->set_short_description( |Temp class for local type handling| ).
        
        " Add class source - this promotes local types to repository level
        lo_specification->definition->add_source( lv_class_source ).

        " Execute the generation - this makes types available to RTTI
        lo_put_operation->execute( ).

      CATCH cx_root INTO DATA(lx_error).
        " Clear result on error
        CLEAR ev_temp_class_name.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_types_with_xco.
    " Extract local type names using XCO string processing
    DATA: lo_string TYPE REF TO if_xco_string,
          lt_lines TYPE string_table.

    " Use XCO string processing capabilities
    lo_string = xco_cp=>string( iv_class_code ).
    lt_lines = lo_string->split( xco_cp_strings=>newline )->value.

    " Simple extraction logic - can be enhanced with XCO regex
    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lo_line) = xco_cp=>string( lv_line ).
      DATA(lv_line_upper) = lo_line->to_upper_case( )->value.

      " Look for TYPES declarations
      IF lv_line_upper CS 'TYPES:' OR lv_line_upper CS 'TYPES '.
        " Extract type name - simplified for demo
        DATA(lv_type_name) = 'EXTRACTED_TYPE'.
        APPEND lv_type_name TO et_type_names.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD cleanup_temp_class.
    " Clean up temporary class using XCO
    TRY.
        DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( '$TMP' )->create_delete_operation( ).
        lo_delete_operation->for_clas->add_object( iv_temp_class_name ).
        lo_delete_operation->execute( ).
        
      CATCH cx_root.
        " Ignore cleanup errors - $TMP objects are temporary anyway
    ENDTRY.
  ENDMETHOD.

  METHOD generate_unique_name.
    " Generate unique temporary class name
    DATA(lv_timestamp) = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    DATA(lv_random) = cl_abap_random_int=>create( seed = lv_timestamp min = 1000 max = 9999 )->get_next( ).
    ev_class_name = |ZCL_TEMP_{ lv_random }|.
  ENDMETHOD.

  METHOD build_temp_class_source.
    " Build complete class source including local types
    DATA: lv_types_section TYPE string,
          lv_methods_section TYPE string.

    " Extract types section from original class
    LOOP AT it_type_names INTO DATA(lv_type_name).
      lv_types_section = lv_types_section && |    TYPES: { lv_type_name } TYPE string.| && cl_abap_char_utilities=>newline.
    ENDLOOP.

    " Extract methods section - simplified
    lv_methods_section = |    METHODS: test_method.| && cl_abap_char_utilities=>newline.

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
                      |  METHOD test_method.| &&
                      cl_abap_char_utilities=>newline &&
                      |  ENDMETHOD.| &&
                      cl_abap_char_utilities=>newline &&
                      |ENDCLASS.|.
  ENDMETHOD.

ENDCLASS.
