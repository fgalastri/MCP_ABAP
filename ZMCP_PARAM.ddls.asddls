@EndUserText.label: 'MCP Method Parameter Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_PARAM {
  @EndUserText.label: 'Parameter Name'
  name      : abap.string(0);
  
  @EndUserText.label: 'Parameter Direction'
  direction : abap.char(10);
  
  @EndUserText.label: 'Parameter Value'
  value     : abap.string(0);
}
