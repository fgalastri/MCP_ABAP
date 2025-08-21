@EndUserText.label: 'MCP Execute Method Result Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_EXECUTE_RESULT {
  @EndUserText.label: 'Request ID'
  request_id    : abap.char(36);
  
  @EndUserText.label: 'Status'
  status        : abap.char(10);
  
  @EndUserText.label: 'Call Result JSON'
  call_result   : abap.string(0);
  
  @EndUserText.label: 'Message'
  message       : abap.string(0);
  
  @EndUserText.label: 'Execution Log'
  execution_log : abap.string(0);
}
