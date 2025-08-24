CLASS zcl_mcp_odata_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_odata_demo.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS call_validation_entity
      IMPORTING
        iv_class_name TYPE string
        iv_class_code TYPE string
        iv_method_name TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS zcl_mcp_odata_demo IMPLEMENTATION.

  METHOD run_odata_demo.
    DATA: lv_class_code TYPE string,
          lv_result TYPE string.

    " Get the actual class code from zcl_metadata_demo
    lv_class_code = |CLASS zcl_metadata_demo DEFINITION| &&
                   | PUBLIC FINAL CREATE PUBLIC .| &&
                   | PUBLIC SECTION.| &&
                   |   METHODS get_mara_record RETURNING VALUE(rs_mara) TYPE mara.| &&
                   | PROTECTED SECTION.| &&
                   | PRIVATE SECTION.| &&
                   |ENDCLASS.| &&
                   |CLASS zcl_metadata_demo IMPLEMENTATION.| &&
                   | METHOD get_mara_record.| &&
                   |   SELECT SINGLE * FROM mara INTO rs_mara.| &&
                   | ENDMETHOD.| &&
                   |ENDCLASS.|.

    WRITE: / '=== Calling ZMCP Validation Service ==='.
    WRITE: / 'Class Name: zcl_metadata_demo'.
    WRITE: / 'Method Name: get_mara_record'.
    WRITE: /.

    " Call validation service
    lv_result = call_validation_entity(
      iv_class_name = 'zcl_metadata_demo'
      iv_class_code = lv_class_code
      iv_method_name = 'get_mara_record'
    ).

    WRITE: / 'Result:'.
    WRITE: / lv_result.

  ENDMETHOD.

  METHOD call_validation_entity.
    " This method would call the ZMCP validation service
    " For demonstration, we'll simulate the call
    
    DATA: lv_request_id TYPE string,
          lv_timestamp TYPE string.

    " Generate request ID
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = lv_request_id.

    " Get timestamp
    GET TIME STAMP FIELD DATA(lv_ts).
    lv_timestamp = |{ lv_ts TIMESTAMP = ISO }|.

    " Simulate the validation call result
    rv_result = |{| &&
               |"REQUEST_ID": "{ lv_request_id }",| &&
               |"STATUS": "SUCCESS",| &&
               |"CLASS_NAME": "{ iv_class_name }",| &&
               |"METHOD_NAME": "{ iv_method_name }",| &&
               |"VALIDATION_TIME": "{ lv_timestamp }",| &&
               |"MESSAGE": "Method validation completed successfully",| &&
               |"CALL_RESULT": {| &&
               |  "METHOD_NAME": "GET_MARA_RECORD",| &&
               |  "PARAMS": [| &&
               |    {| &&
               |      "NAME": "RS_MARA",| &&
               |      "DIRECTION": "RETURNING",| &&
               |      "TYPE": "MARA",| &&
               |      "VALUE": "Material record retrieved"| &&
               |    }| &&
               |  ]| &&
               |}| &&
               |}|.

    " In a real implementation, you would:
    " 1. Create an instance of ZCL_CE_MCP_METHOD_VALIDATE
    " 2. Call the validation method with proper parameters
    " 3. Handle the response and format the result

  ENDMETHOD.

ENDCLASS.

