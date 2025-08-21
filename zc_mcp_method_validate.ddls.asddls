@EndUserText.label: 'MCP Method Validation Custom Entity'

@ObjectModel.query.implementedBy: 'ABAP:ZCL_CE_MCP_METHOD_VALIDATE'

@UI.headerInfo: { typeName: 'Method Validation', typeNamePlural: 'Method Validations' }

define root custom entity ZC_MCP_METHOD_VALIDATE

{
      @UI.facet: [ { id: 'GeneralInfo', type: #COLLECTION, label: 'General Information' },
                   { id: 'General', type: #IDENTIFICATION_REFERENCE, label: 'General', parentId: 'GeneralInfo' } ]

      @EndUserText.label: 'Request ID'
      @UI.identification: [ { position: 10 } ]
      @UI.lineItem: [ { position: 10, importance: #HIGH } ]
  key request_id          : abap.char(36);

      @EndUserText.label: 'Class Name'
      @UI.identification: [ { position: 20 } ]
      @UI.lineItem: [ { position: 20, importance: #HIGH } ]
      class_name          : abap.char(30);

      @EndUserText.label: 'Class Code'
      @UI.identification: [ { position: 30 } ]
      @UI.lineItem: [ { position: 30, importance: #MEDIUM } ]
      class_code          : abap.string(0);

      @EndUserText.label: 'Method Calls'
      @UI.identification: [ { position: 40 } ]
      @UI.lineItem: [ { position: 40, importance: #MEDIUM } ]
      method_calls        : abap.string(0);

      @EndUserText.label: 'Status'
      @UI.identification: [ { position: 50 } ]
      @UI.lineItem: [ { position: 50, importance: #HIGH } ]
      status              : abap.char(10);

      @EndUserText.label: 'Result'
      @UI.identification: [ { position: 60 } ]
      @UI.lineItem: [ { position: 60, importance: #MEDIUM } ]
      call_result              : abap.string(0);

      @EndUserText.label: 'Message'
      @UI.identification: [ { position: 70 } ]
      @UI.lineItem: [ { position: 70, importance: #MEDIUM } ]
      message             : abap.string(0);

      @EndUserText.label: 'Created At'
      @UI.identification: [ { position: 80 } ]
      @UI.lineItem: [ { position: 80, importance: #LOW } ]
      created_at          : abap.utclong;
}
