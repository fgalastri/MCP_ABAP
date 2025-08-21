CLASS zcl_ce_mcp_method_validate DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC 
  INHERITING FROM cl_rap_query_provider.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    
    TYPES: BEGIN OF ty_method_validate,
             client         TYPE sy-mandt,
             request_id     TYPE c LENGTH 36,
             class_name     TYPE c LENGTH 30,
             class_code     TYPE string,
             method_calls   TYPE string,
             status         TYPE c LENGTH 10,
             call_result    TYPE string,
             message        TYPE string,
             created_at     TYPE utclong,
           END OF ty_method_validate.
    
    TYPES tt_method_validate TYPE TABLE OF ty_method_validate WITH EMPTY KEY.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_ce_mcp_method_validate IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    
    DATA lt_data TYPE tt_method_validate.
    DATA ls_data TYPE ty_method_validate.
    
    " Since this is a custom entity for actions, we return minimal data
    " The actual processing happens in the behavior class actions
    
    " Get filter criteria if any
    DATA(lt_filter_cond) = io_request->get_filter( )->get_as_ranges( ).
    
    " For demo purposes, create a sample entry
    ls_data-client = sy-mandt.
    ls_data-request_id = cl_system_uuid=>create_uuid_x16_static( ).
    ls_data-class_name = 'Sample'.
    ls_data-status = 'READY'.
    ls_data-created_at = utclong_current( ).
    
    APPEND ls_data TO lt_data.
    
    " Apply requested aggregations if any
    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records( lines( lt_data ) ).
    ENDIF.
    
    " Apply paging if requested
    DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
    DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
    
    " Set the response data
    io_response->set_data( lt_data ).
    
  ENDMETHOD.

ENDCLASS.
