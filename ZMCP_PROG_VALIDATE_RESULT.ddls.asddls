@EndUserText.label: 'MCP Program Validate Result Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_PROG_VALIDATE_RESULT {
  @EndUserText.label: 'Request ID'
  request_id     : abap.char(36);
  
  @EndUserText.label: 'Program Name'
  program_name   : abap.string(0);
  
  @EndUserText.label: 'Status'
  status         : abap.char(10);
  
  @EndUserText.label: 'Message'
  message        : abap.string(0);
  
  @EndUserText.label: 'Error Line'
  error_line     : abap.int4;
  
  @EndUserText.label: 'Error Message'
  error_message  : abap.string(0);
}
