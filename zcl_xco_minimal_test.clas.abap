CLASS zcl_xco_minimal_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             message TYPE string,
           END OF ty_result.

    METHODS test_xco_availability
               EXPORTING es_result TYPE ty_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_xco_minimal_test IMPLEMENTATION.

  METHOD test_xco_availability.
    " Test if XCO library is available in the environment
    es_result-success = abap_false.

    TRY.
        " Try to use basic XCO string functionality
        DATA(lo_string) = xco_cp=>string( 'test' ).
        DATA(lv_upper) = lo_string->to_upper_case( )->value.
        
        IF lv_upper = 'TEST'.
          es_result-success = abap_true.
          es_result-message = 'XCO library is available and working'.
        ELSE.
          es_result-message = 'XCO string processing not working correctly'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        es_result-message = |XCO not available: { lx_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

