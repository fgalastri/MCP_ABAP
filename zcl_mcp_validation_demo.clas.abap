CLASS zcl_mcp_validation_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_validation_demo.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS call_mcp_validation_service
      IMPORTING
        iv_class_name TYPE string
        iv_class_code TYPE string
        iv_method_name TYPE string OPTIONAL
        it_parameters TYPE string_table OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS zcl_mcp_validation_demo IMPLEMENTATION.

  METHOD run_validation_demo.
    DATA: lv_class_code TYPE string,
          lv_result TYPE string,
          lt_parameters TYPE string_table.

    " Define the class code for zcl_metadata_demo
    lv_class_code = |CLASS zcl_metadata_demo DEFINITION| &&
                   | PUBLIC| &&
                   | FINAL| &&
                   | CREATE PUBLIC .| &&
                   || &&
                   | PUBLIC SECTION.| &&
                   |   METHODS run_demo.| &&
                   |   METHODS get_mara_record| &&
                   |     RETURNING| &&
                   |       VALUE(rs_mara) TYPE mara.| &&
                   |   METHODS get_material_number| &&
                   |     RETURNING| &&
                   |       VALUE(rv_matnr) TYPE matnr.| &&
                   || &&
                   | PROTECTED SECTION.| &&
                   | PRIVATE SECTION.| &&
                   |   TYPES ty_title TYPE c LENGTH 50.| &&
                   |   DATA mo_metadata_service TYPE REF TO zcl_metadata_service.| &&
                   || &&
                   |   METHODS demo_cds_fields.| &&
                   |   METHODS demo_table_fields.| &&
                   |   METHODS demo_class_methods.| &&
                   |   METHODS demo_functions.| &&
                   |   METHODS test_mara_selection.| &&
                   |   METHODS write_results| &&
                   |     IMPORTING| &&
                   |       iv_title TYPE ty_title| &&
                   |       it_results TYPE ANY TABLE.| &&
                   |ENDCLASS.| &&
                   || &&
                   |CLASS zcl_metadata_demo IMPLEMENTATION.| &&
                   || &&
                   | METHOD run_demo.| &&
                   |   " Initialize metadata service| &&
                   |   CREATE OBJECT mo_metadata_service.| &&
                   || &&
                   |   " Run demonstrations| &&
                   |   demo_cds_fields( ).| &&
                   |   demo_table_fields( ).| &&
                   |   demo_class_methods( ).| &&
                   |   demo_functions( ).| &&
                   |   test_mara_selection( ).| &&
                   | ENDMETHOD.| &&
                   || &&
                   | METHOD get_mara_record.| &&
                   |   " Select one record from MARA table with all fields| &&
                   |   SELECT SINGLE *| &&
                   |     FROM mara| &&
                   |     INTO rs_mara.| &&
                   | ENDMETHOD.| &&
                   || &&
                   | METHOD get_material_number.| &&
                   |   " Select one material number from MARA table| &&
                   |   SELECT SINGLE matnr| &&
                   |     FROM mara| &&
                   |     INTO rv_matnr.| &&
                   | ENDMETHOD.| &&
                   || &&
                   | METHOD test_mara_selection.| &&
                   |   " Test the MARA selection method| &&
                   |   DATA: lv_matnr TYPE matnr.| &&
                   |   lv_matnr = get_mara_record( )-matnr.| &&
                   |   IF lv_matnr IS NOT INITIAL.| &&
                   |     " Record found| &&
                   |   ELSE.| &&
                   |     " No record found| &&
                   |   ENDIF.| &&
                   | ENDMETHOD.| &&
                   || &&
                   |ENDCLASS.|.

    " Prepare parameters for get_mara_record method call
    APPEND '{"name": "rs_mara", "direction": "RETURNING", "type": "mara", "value": ""}' TO lt_parameters.

    " Call the MCP validation service for syntax check first
    WRITE: / '=== Syntax Validation ==='.
    lv_result = call_mcp_validation_service(
      iv_class_name = 'zcl_metadata_demo'
      iv_class_code = lv_class_code
    ).
    WRITE: / 'Syntax Result:', lv_result.

    " Call the MCP validation service for method execution
    WRITE: / '=== Method Execution ==='.
    lv_result = call_mcp_validation_service(
      iv_class_name = 'zcl_metadata_demo'
      iv_class_code = lv_class_code
      iv_method_name = 'get_mara_record'
      it_parameters = lt_parameters
    ).
    WRITE: / 'Execution Result:', lv_result.

  ENDMETHOD.

  METHOD call_mcp_validation_service.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_url TYPE string,
          lv_request_body TYPE string,
          lv_response TYPE string,
          lv_status_code TYPE i.

    " Build the request URL (adjust to your system)
    lv_url = '/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/ZMCP_VALIDATE_RESULT'.

    " Build JSON request body
    lv_request_body = |{|.
    lv_request_body = lv_request_body && |"CLASS_NAME": "| && iv_class_name && |",|.
    lv_request_body = lv_request_body && |"CLASS_CODE": "| && iv_class_code && |"|.
    
    IF iv_method_name IS NOT INITIAL.
      lv_request_body = lv_request_body && |,"METHOD_NAME": "| && iv_method_name && |"|.
    ENDIF.
    
    IF lines( it_parameters ) > 0.
      lv_request_body = lv_request_body && |,"PARAMETERS": [|.
      LOOP AT it_parameters INTO DATA(lv_param).
        IF sy-tabix > 1.
          lv_request_body = lv_request_body && |,|.
        ENDIF.
        lv_request_body = lv_request_body && lv_param.
      ENDLOOP.
      lv_request_body = lv_request_body && |]|.
    ENDIF.
    
    lv_request_body = lv_request_body && |}|.

    TRY.
        " Create HTTP client
        CALL METHOD cl_http_client=>create_by_url
          EXPORTING
            url    = lv_url
          IMPORTING
            client = lo_http_client.

        " Set request method and headers
        lo_http_client->request->set_method( 'POST' ).
        lo_http_client->request->set_header_field(
          name  = 'Content-Type'
          value = 'application/json'
        ).
        lo_http_client->request->set_header_field(
          name  = 'Accept'
          value = 'application/json'
        ).

        " Set request body
        lo_http_client->request->set_cdata( lv_request_body ).

        " Send request
        lo_http_client->send( ).

        " Receive response
        lo_http_client->receive( ).

        " Get response
        lv_response = lo_http_client->response->get_cdata( ).
        lv_status_code = lo_http_client->response->get_status( )-code.

        " Close connection
        lo_http_client->close( ).

        " Return result
        rv_result = |Status: { lv_status_code }, Response: { lv_response }|.

      CATCH cx_root INTO DATA(lx_error).
        rv_result = |Error: { lx_error->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

