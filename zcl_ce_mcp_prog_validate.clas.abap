CLASS zcl_ce_mcp_prog_validate DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC 
  INHERITING FROM cl_rap_query_provider.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    
    TYPES: BEGIN OF ty_program_validate,
             client         TYPE sy-mandt,
             request_id     TYPE c LENGTH 36,
             program_name   TYPE c LENGTH 40,
             source_code    TYPE string,
             status         TYPE c LENGTH 10,
             message        TYPE string,
             error_line     TYPE i,
             error_message  TYPE string,
             created_at     TYPE utclong,
           END OF ty_program_validate.
    
    TYPES tt_program_validate TYPE TABLE OF ty_program_validate WITH EMPTY KEY.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_ce_mcp_prog_validate IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    " Simple implementation - return empty result
    " Actions will handle the actual validation logic
    DATA lt_result TYPE tt_program_validate.
    io_response->set_data( lt_result ).
  ENDMETHOD.

ENDCLASS.
