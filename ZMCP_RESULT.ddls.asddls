@EndUserText.label: 'MCP Method Result Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_RESULT {
  @EndUserText.label: 'Method Name'
  method_name : abap.string(0);
  
  @EndUserText.label: 'Parameter Name'
  param_name  : abap.string(0);
  
  @EndUserText.label: 'Parameter Value'
  value       : abap.string(0);
}
