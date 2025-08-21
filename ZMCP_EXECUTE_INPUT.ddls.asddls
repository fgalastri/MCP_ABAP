@EndUserText.label: 'MCP Execute Method Input Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_EXECUTE_INPUT {
  @EndUserText.label: 'Class Name'
  class_name   : abap.string(0);
  
  @EndUserText.label: 'Class Code'
  class_code   : abap.string(0);
  
  @EndUserText.label: 'Method Calls JSON'
  method_calls : abap.string(0);
}
