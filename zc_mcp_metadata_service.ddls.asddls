@EndUserText.label: 'MCP Metadata Service Custom Entity'

@ObjectModel.query.implementedBy: 'ABAP:ZCL_CE_MCP_META_SVC'

@UI.headerInfo: { typeName: 'Metadata Request', typeNamePlural: 'Metadata Requests' }

define root custom entity ZC_MCP_METADATA_SERVICE

{
      @UI.facet: [ { id: 'GeneralInfo', type: #COLLECTION, label: 'General Information' },
                   { id: 'General', type: #IDENTIFICATION_REFERENCE, label: 'General', parentId: 'GeneralInfo' } ]

      @EndUserText.label: 'Request ID'
      @UI.identification: [ { position: 10 } ]
      @UI.lineItem: [ { position: 10, importance: #HIGH } ]
  key request_id          : abap.char(36);

      @EndUserText.label: 'Request Type'
      @UI.identification: [ { position: 20 } ]
      @UI.lineItem: [ { position: 20, importance: #HIGH } ]
      request_type        : abap.char(20);

      @EndUserText.label: 'Object Names'
      @UI.identification: [ { position: 30 } ]
      @UI.lineItem: [ { position: 30, importance: #MEDIUM } ]
      object_names        : abap.string(0);

      @EndUserText.label: 'Status'
      @UI.identification: [ { position: 40 } ]
      @UI.lineItem: [ { position: 40, importance: #HIGH } ]
      status              : abap.char(10);

      @EndUserText.label: 'Result Data'
      @UI.identification: [ { position: 50 } ]
      @UI.lineItem: [ { position: 50, importance: #MEDIUM } ]
      result_data         : abap.string(0);

      @EndUserText.label: 'Message'
      @UI.identification: [ { position: 60 } ]
      @UI.lineItem: [ { position: 60, importance: #MEDIUM } ]
      message             : abap.string(0);

      @EndUserText.label: 'Created At'
      @UI.identification: [ { position: 70 } ]
      @UI.lineItem: [ { position: 70, importance: #LOW } ]
      created_at          : abap.dats;
}