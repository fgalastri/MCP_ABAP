@EndUserText.label: 'MCP Program Validation Custom Entity'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CE_MCP_PROG_VALIDATE'

@UI.headerInfo: { typeName: 'Program Validation', typeNamePlural: 'Program Validations' }

define root custom entity ZC_MCP_PROGRAM_VALIDATE

{
      @UI.facet: [ { id: 'GeneralInfo', type: #COLLECTION, label: 'General Information' },
                   { id: 'General', type: #IDENTIFICATION_REFERENCE, label: 'General', parentId: 'GeneralInfo' },
                   { id: 'ValidationInfo', type: #COLLECTION, label: 'Validation Details' },
                   { id: 'Validation', type: #IDENTIFICATION_REFERENCE, label: 'Validation', parentId: 'ValidationInfo' } ]

      @EndUserText.label: 'Request ID'
      @UI.identification: [ { position: 10 } ]
      @UI.lineItem: [ { position: 10, importance: #HIGH } ]
  key request_id          : abap.char(36);

      @EndUserText.label: 'Program Name'
      @UI.identification: [ { position: 20 } ]
      @UI.lineItem: [ { position: 20, importance: #HIGH } ]
      @UI.selectionField: [ { position: 10 } ]
      program_name        : abap.char(40);

      @EndUserText.label: 'Source Code'
      @UI.identification: [ { position: 30 } ]
      @UI.multiLineText: true
      source_code         : abap.string(0);

      @EndUserText.label: 'Status'
      @UI.identification: [ { position: 40 } ]
      @UI.lineItem: [ { position: 30, importance: #HIGH } ]
      @UI.selectionField: [ { position: 20 } ]
      status              : abap.char(10);    // 'SUCCESS', 'ERROR'

      @EndUserText.label: 'Message'
      @UI.identification: [ { position: 50 } ]
      @UI.lineItem: [ { position: 40, importance: #MEDIUM } ]
      message             : abap.string(0);

      @EndUserText.label: 'Error Line'
      @UI.identification: [ { position: 60 } ]
      @UI.lineItem: [ { position: 50, importance: #MEDIUM } ]
      error_line          : abap.int4;

      @EndUserText.label: 'Error Message'
      @UI.identification: [ { position: 70 } ]
      @UI.lineItem: [ { position: 60, importance: #MEDIUM } ]
      error_message       : abap.string(0);

      @EndUserText.label: 'Created At'
      @UI.identification: [ { position: 80 } ]
      @UI.lineItem: [ { position: 70, importance: #LOW } ]
      created_at          : abap.utclong;

}
