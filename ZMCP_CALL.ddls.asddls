@EndUserText.label: 'MCP Method Call Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_CALL {
  @EndUserText.label: 'Method Name'
  method_name : abap.string(0);
  
  @EndUserText.label: 'Method Parameters'
  params      : ZMCP_PARAM_TAB;
}
