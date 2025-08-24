@EndUserText.label: 'MCP Program Validate Input Structure'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE

define structure ZMCP_PROG_VALIDATE_INPUT {
  @EndUserText.label: 'Program Name'
  program_name : abap.string(0);
  
  @EndUserText.label: 'Source Code'
  source_code  : abap.string(0);
}
