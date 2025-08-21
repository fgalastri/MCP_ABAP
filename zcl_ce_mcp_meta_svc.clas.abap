CLASS zcl_ce_mcp_meta_svc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_metadata_result,
             request_id    TYPE c LENGTH 36,
             request_type  TYPE c LENGTH 20,
             object_names  TYPE string,
             status        TYPE c LENGTH 10,
             result_data   TYPE string,
             message       TYPE string,
             created_at    TYPE dats,
           END OF ty_metadata_result,
           ty_metadata_results TYPE TABLE OF ty_metadata_result.

    METHODS get_cds_metadata
      IMPORTING iv_object_names TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_table_metadata
      IMPORTING iv_object_names TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    TYPES: ty_object_name TYPE c LENGTH 30,
           tt_object_names TYPE TABLE OF ty_object_name WITH DEFAULT KEY.
    
    METHODS parse_object_names
      IMPORTING iv_names TYPE string
      RETURNING VALUE(rt_names) TYPE tt_object_names.
ENDCLASS.

CLASS zcl_ce_mcp_meta_svc IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    DATA: lt_result TYPE ty_metadata_results.

    " Get filter values
    DATA(lt_filter_conditions) = io_request->get_filter( )->get_as_ranges( ).

    " Extract filters
    DATA: lv_request_type TYPE string,
          lv_object_names TYPE string.

    READ TABLE lt_filter_conditions WITH KEY name = 'request_type' INTO DATA(ls_request_type).
    IF sy-subrc = 0 AND lines( ls_request_type-range ) > 0.
      lv_request_type = ls_request_type-range[ 1 ]-low.
    ENDIF.

    READ TABLE lt_filter_conditions WITH KEY name = 'object_names' INTO DATA(ls_object_names).
    IF sy-subrc = 0 AND lines( ls_object_names-range ) > 0.
      lv_object_names = ls_object_names-range[ 1 ]-low.
    ENDIF.

    " Generate unique request ID
    DATA(lv_request_id) = cl_system_uuid=>create_uuid_x16_static( ).

    " Create result entry
    DATA: ls_result TYPE ty_metadata_result.
    ls_result-request_id = lv_request_id.
    ls_result-request_type = lv_request_type.
    ls_result-object_names = lv_object_names.
    ls_result-created_at = sy-datum.

    " Process based on request type
    CASE lv_request_type.
      WHEN 'CDS_SOURCE'.
        TRY.
            ls_result-result_data = get_cds_metadata( lv_object_names ).
            ls_result-status = 'SUCCESS'.
            ls_result-message = 'CDS metadata retrieved successfully'.
          CATCH cx_root INTO DATA(lx_cds_error).
            ls_result-status = 'ERROR'.
            ls_result-message = lx_cds_error->get_text( ).
        ENDTRY.

      WHEN 'TABLE_FIELDS'.
        TRY.
            ls_result-result_data = get_table_metadata( lv_object_names ).
            ls_result-status = 'SUCCESS'.
            ls_result-message = 'Table metadata retrieved successfully'.
          CATCH cx_root INTO DATA(lx_table_error).
            ls_result-status = 'ERROR'.
            ls_result-message = lx_table_error->get_text( ).
        ENDTRY.

      WHEN OTHERS.
        ls_result-status = 'ERROR'.
        ls_result-message = 'Invalid request type. Use CDS_SOURCE or TABLE_FIELDS'.
    ENDCASE.

    APPEND ls_result TO lt_result.

    " Set total number of records if requested
    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records( lines( lt_result ) ).
    ENDIF.

    " Return result
    io_response->set_data( lt_result ).
  ENDMETHOD.

  METHOD get_cds_metadata.
    DATA: lo_metadata TYPE REF TO zcl_metadata_service.
    CREATE OBJECT lo_metadata.

    " Parse object names from JSON  
    DATA(lt_object_names) = parse_object_names( iv_object_names ).
    DATA: lt_cds_names TYPE zcl_metadata_service=>ddlname_tab.
    
    " Convert string table to ddlname_tab
    LOOP AT lt_object_names INTO DATA(lv_name).
      APPEND lv_name TO lt_cds_names.
    ENDLOOP.

    " Get CDS sources
    DATA(lt_sources) = lo_metadata->get_cds_source( lt_cds_names ).

    " Convert to JSON
    DATA: lv_json TYPE string.
    lv_json = '['.

    LOOP AT lt_sources INTO DATA(ls_source).
      IF sy-tabix > 1.
        lv_json = lv_json && ','.
      ENDIF.

      " Escape special characters in source code
      DATA(lv_escaped_source) = ls_source-source.
      REPLACE ALL OCCURRENCES OF '\' IN lv_escaped_source WITH '\\'.
      REPLACE ALL OCCURRENCES OF '"' IN lv_escaped_source WITH '\"'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_escaped_source WITH '\n'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_escaped_source WITH '\n'.

      lv_json = lv_json && '{"cds_name":"' && ls_source-cds_name && '","source":"' && lv_escaped_source && '"}'.
    ENDLOOP.

    lv_json = lv_json && ']'.
    rv_result = lv_json.
  ENDMETHOD.

  METHOD get_table_metadata.
    DATA: lo_metadata TYPE REF TO zcl_metadata_service.
    CREATE OBJECT lo_metadata.

    " Parse object names from JSON
    DATA(lt_object_names) = parse_object_names( iv_object_names ).
    DATA: lt_table_names TYPE zcl_metadata_service=>ddlname_tab.
    
    " Convert string table to ddlname_tab  
    LOOP AT lt_object_names INTO DATA(lv_name).
      APPEND lv_name TO lt_table_names.
    ENDLOOP.

    " Get table fields
    DATA(lt_fields) = lo_metadata->get_table_stru_fields( lt_table_names ).

    " Convert to JSON
    DATA: lv_json TYPE string.
    lv_json = '['.

    LOOP AT lt_fields INTO DATA(ls_field).
      IF sy-tabix > 1.
        lv_json = lv_json && ','.
      ENDIF.

      lv_json = lv_json && '{"tab_name":"' && ls_field-tab_name && '",'
                        && '"field_name":"' && ls_field-field_name && '",'
                        && '"key_flag":"' && ls_field-key_flag && '",'
                        && '"rollname":"' && ls_field-rollname && '",'
                        && '"domain_name":"' && ls_field-domain_name && '",'
                        && '"abap_dtype":"' && ls_field-abap_dtype && '",'
                        && '"db_dtype":"' && ls_field-db_dtype && '",'
                        && '"length":"' && ls_field-length && '",'
                        && '"decimals":"' && ls_field-decimals && '",'
                        && '"description":"' && ls_field-description && '"}'.
    ENDLOOP.

    lv_json = lv_json && ']'.
    rv_result = lv_json.
  ENDMETHOD.

  METHOD parse_object_names.
    " Simple JSON array parser for ["name1","name2","name3"]
    DATA: lv_names TYPE string.
    lv_names = iv_names.

    " Remove brackets and quotes
    REPLACE ALL OCCURRENCES OF '[' IN lv_names WITH ''.
    REPLACE ALL OCCURRENCES OF ']' IN lv_names WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN lv_names WITH ''.

    " Split by comma and add to table
    SPLIT lv_names AT ',' INTO TABLE DATA(lt_name_strings).

    LOOP AT lt_name_strings INTO DATA(lv_name).
      " Trim spaces
      CONDENSE lv_name.
      IF lv_name IS NOT INITIAL.
        APPEND lv_name TO rt_names.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
