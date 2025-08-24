CLASS zcl_metadata_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_demo.
    METHODS get_mara_record
      RETURNING
        VALUE(rs_mara) TYPE mara.
    METHODS get_material_number
      RETURNING
        VALUE(rv_matnr) TYPE matnr.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_title TYPE c LENGTH 50.
    DATA mo_metadata_service TYPE REF TO zcl_metadata_service.

    METHODS demo_cds_fields.
    METHODS demo_table_fields.
    METHODS demo_class_methods.
    METHODS demo_functions.
    METHODS test_mara_selection.
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
    test_mara_selection( ).
  ENDMETHOD.

  METHOD demo_cds_fields.
    " Demonstrate CDS fields retrieval
    DATA lt_cds_names TYPE zcl_metadata_service=>ddlname_tab.
    DATA ls_cds_name TYPE sxco_cds_object_name.
    DATA lt_results TYPE zcl_metadata_service=>ty_cds_source_tab.

    " Prepare test data
    ls_cds_name = 'ZV_CUSTOMER_DATA'.
    APPEND ls_cds_name TO lt_cds_names.
    ls_cds_name = 'ZV_MATERIAL_INFO'.
    APPEND ls_cds_name TO lt_cds_names.

    " Get CDS source metadata
    lt_results = mo_metadata_service->get_cds_source( lt_cds_names ).

    " Display results
    write_results(
      iv_title = 'CDS Fields Metadata'
      it_results = lt_results
    ).
  ENDMETHOD.

  METHOD demo_table_fields.
    " Demonstrate table fields retrieval
    DATA lt_table_names TYPE zcl_metadata_service=>ddlname_tab.
    DATA ls_table_name TYPE sxco_cds_object_name.
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
    DATA lt_class_names TYPE zcl_metadata_service=>ddlname_tab.
    DATA ls_class_name TYPE sxco_cds_object_name.
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
    DATA lt_func_names TYPE zcl_metadata_service=>ddlname_tab.
    DATA ls_func_name TYPE sxco_cds_object_name.
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

  METHOD get_mara_record.
    " Select one record from MARA table with all fields
    SELECT SINGLE *
      FROM mara
      INTO rs_mara.
  ENDMETHOD.

  METHOD get_material_number.
    " Select one material number from MARA table
    SELECT SINGLE matnr
      FROM mara
      INTO rv_matnr.
  ENDMETHOD.

  METHOD test_mara_selection.
    " Test the MARA selection method
    " Call the method with RETURNING parameter and check result directly
    DATA: lv_matnr TYPE matnr.
    
    " Get the result and extract material number for checking
    lv_matnr = get_mara_record( )-matnr.
    
    " Check if record was found
    IF lv_matnr IS NOT INITIAL.
      " Record found - in real implementation, process the data
      " For demo purposes, we just verify it's not empty
    ELSE.
      " No record found
    ENDIF.
  ENDMETHOD.

ENDCLASS.
