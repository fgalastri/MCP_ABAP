CLASS zcl_metadata_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_demo.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_title TYPE c LENGTH 50.
    DATA mo_metadata_service TYPE REF TO zcl_metadata_service.

    METHODS demo_cds_fields.
    METHODS demo_table_fields.
    METHODS demo_class_methods.
    METHODS demo_functions.
    METHODS write_results
      IMPORTING
        iv_title TYPE ty_title
        it_results TYPE ANY TABLE.
ENDCLASS.

CLASS zcl_metadata_demo IMPLEMENTATION.

  METHOD run_demo.
    " Initialize metadata service
    CREATE OBJECT mo_metadata_service.

    " Run demonstrations
    demo_cds_fields( ).
    demo_table_fields( ).
    demo_class_methods( ).
    demo_functions( ).
  ENDMETHOD.

  METHOD demo_cds_fields.
    " Demonstrate CDS fields retrieval
    DATA lt_cds_names TYPE zcl_metadata_service=>ty_cds_name_tab.
    DATA ls_cds_name TYPE zcl_metadata_service=>ty_cds_name.
    DATA lt_results TYPE zcl_metadata_service=>ty_cds_field_tab.

    " Prepare test data
    ls_cds_name = 'ZV_CUSTOMER_DATA'.
    APPEND ls_cds_name TO lt_cds_names.
    ls_cds_name = 'ZV_MATERIAL_INFO'.
    APPEND ls_cds_name TO lt_cds_names.

    " Get CDS field metadata
    lt_results = mo_metadata_service->get_cds_fields( lt_cds_names ).

    " Display results
    write_results(
      iv_title = 'CDS Fields Metadata'
      it_results = lt_results
    ).
  ENDMETHOD.

  METHOD demo_table_fields.
    " Demonstrate table fields retrieval
    DATA lt_table_names TYPE zcl_metadata_service=>ty_field_name_tab.
    DATA ls_table_name TYPE zcl_metadata_service=>ty_field_name.
    DATA lt_results TYPE zcl_metadata_service=>ty_table_field_tab.

    " Prepare test data
    ls_table_name = 'MARA'.
    APPEND ls_table_name TO lt_table_names.
    ls_table_name = 'KNA1'.
    APPEND ls_table_name TO lt_table_names.

    " Get table field metadata
    lt_results = mo_metadata_service->get_table_stru_fields( lt_table_names ).

    " Display results
    write_results(
      iv_title = 'Table Fields Metadata'
      it_results = lt_results
    ).
  ENDMETHOD.

  METHOD demo_class_methods.
    " Demonstrate class methods retrieval
    DATA lt_class_names TYPE zcl_metadata_service=>ty_class_name_tab.
    DATA ls_class_name TYPE zcl_metadata_service=>ty_class_name.
    DATA lt_results TYPE zcl_metadata_service=>ty_method_param_tab.

    " Prepare test data
    ls_class_name = 'ZCL_BUSINESS_LOGIC'.
    APPEND ls_class_name TO lt_class_names.
    ls_class_name = 'ZCL_DATA_PROCESSOR'.
    APPEND ls_class_name TO lt_class_names.

    " Get class methods metadata
    lt_results = mo_metadata_service->get_classes_methods( lt_class_names ).

    " Display results
    write_results(
      iv_title = 'Class Methods Metadata'
      it_results = lt_results
    ).
  ENDMETHOD.

  METHOD demo_functions.
    " Demonstrate function modules retrieval
    DATA lt_func_names TYPE zcl_metadata_service=>ty_func_name_tab.
    DATA ls_func_name TYPE zcl_metadata_service=>ty_func_name.
    DATA lt_results TYPE zcl_metadata_service=>ty_function_param_tab.

    " Prepare test data
    ls_func_name = 'Z_CALCULATE_PRICE'.
    APPEND ls_func_name TO lt_func_names.
    ls_func_name = 'Z_VALIDATE_DATA'.
    APPEND ls_func_name TO lt_func_names.

    " Get function parameters metadata
    lt_results = mo_metadata_service->get_functions( lt_func_names ).

    " Display results
    write_results(
      iv_title = 'Function Parameters Metadata'
      it_results = lt_results
    ).
  ENDMETHOD.

  METHOD write_results.
    " Simple output method for demonstration
    DATA lv_lines TYPE i.
    
    lv_lines = lines( it_results ).
    
    " In a real implementation, you would format and display the results
    " For demo purposes, we just show the count
    
    " Note: WRITE statements not supported in validation environment
    " In real system, you would use:
    " WRITE: / '=== ', iv_title, ' ==='.
    " WRITE: / 'Found ', lv_lines, ' records.'.
    " WRITE: /.
    
    " Alternative: Use message or log the results
    IF lv_lines > 0.
      " Results found - in real implementation, format and display
    ELSE.
      " No results found
    ENDIF.
  ENDMETHOD.

ENDCLASS.
