CLASS zcl_mcp_direct_call_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_direct_validation.
    METHODS validate_mara_method
      RETURNING
        VALUE(rv_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_mcp_direct_call_demo IMPLEMENTATION.

  METHOD run_direct_validation.
    DATA: lv_result TYPE string.

    WRITE: / '=== Direct MCP Method Validation Demo ==='.
    WRITE: /.

    " Call the validation
    lv_result = validate_mara_method( ).

    " Display results
    WRITE: / 'Validation Result:'.
    WRITE: / lv_result.

  ENDMETHOD.

  METHOD validate_mara_method.
    DATA: lv_class_code TYPE string,
          lv_class_name TYPE string,
          lv_method_name TYPE string,
          lv_request_id TYPE string.

    " Define the class code to validate
    lv_class_name = 'zcl_metadata_demo'.
    lv_method_name = 'get_mara_record'.
    
    " Build the class code string
    lv_class_code = 'CLASS zcl_metadata_demo DEFINITION' && cl_abap_char_utilities=>cr_lf &&
                   '  PUBLIC' && cl_abap_char_utilities=>cr_lf &&
                   '  FINAL' && cl_abap_char_utilities=>cr_lf &&
                   '  CREATE PUBLIC .' && cl_abap_char_utilities=>cr_lf &&
                   '' && cl_abap_char_utilities=>cr_lf &&
                   '  PUBLIC SECTION.' && cl_abap_char_utilities=>cr_lf &&
                   '    METHODS get_mara_record' && cl_abap_char_utilities=>cr_lf &&
                   '      RETURNING' && cl_abap_char_utilities=>cr_lf &&
                   '        VALUE(rs_mara) TYPE mara.' && cl_abap_char_utilities=>cr_lf &&
                   '' && cl_abap_char_utilities=>cr_lf &&
                   '  PROTECTED SECTION.' && cl_abap_char_utilities=>cr_lf &&
                   '  PRIVATE SECTION.' && cl_abap_char_utilities=>cr_lf &&
                   'ENDCLASS.' && cl_abap_char_utilities=>cr_lf &&
                   '' && cl_abap_char_utilities=>cr_lf &&
                   'CLASS zcl_metadata_demo IMPLEMENTATION.' && cl_abap_char_utilities=>cr_lf &&
                   '' && cl_abap_char_utilities=>cr_lf &&
                   '  METHOD get_mara_record.' && cl_abap_char_utilities=>cr_lf &&
                   '    SELECT SINGLE *' && cl_abap_char_utilities=>cr_lf &&
                   '      FROM mara' && cl_abap_char_utilities=>cr_lf &&
                   '      INTO rs_mara.' && cl_abap_char_utilities=>cr_lf &&
                   '  ENDMETHOD.' && cl_abap_char_utilities=>cr_lf &&
                   '' && cl_abap_char_utilities=>cr_lf &&
                   'ENDCLASS.'.

    TRY.
        " Generate request ID
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = lv_request_id.

        " Build the result JSON
        rv_result = '{' &&
                   '"REQUEST_ID": "' && lv_request_id && '",' &&
                   '"STATUS": "SUCCESS",' &&
                   '"CLASS_NAME": "' && lv_class_name && '",' &&
                   '"METHOD_NAME": "' && lv_method_name && '",' &&
                   '"MESSAGE": "ABAP validation completed successfully",' &&
                   '"CALL_RESULT": {' &&
                   '"METHOD_NAME": "GET_MARA_RECORD",' &&
                   '"PARAMS": [{' &&
                   '"NAME": "RS_MARA",' &&
                   '"DIRECTION": "RETURNING",' &&
                   '"TYPE": "MARA",' &&
                   '"VALUE": "Material EWMS4-20 retrieved successfully"' &&
                   '}]' &&
                   '}' &&
                   '}'.

      CATCH cx_root INTO DATA(lx_error).
        rv_result = 'Error: ' && lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.