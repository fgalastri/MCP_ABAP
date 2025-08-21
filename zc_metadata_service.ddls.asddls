@EndUserText.label: 'Metadata Service Custom Entity'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CE_METADATA_SERVICE'

define root custom entity ZC_METADATA_SERVICE
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
      @UI.selectionField: [ { position: 10 } ]
      request_type        : abap.char(20);    // 'CDS_SOURCE' or 'TABLE_FIELDS'

      @EndUserText.label: 'Object Names'
      @UI.identification: [ { position: 30 } ]
      @UI.lineItem: [ { position: 30, importance: #MEDIUM } ]
      @UI.selectionField: [ { position: 20 } ]
      object_names        : abap.string(0);   // JSON array of names

      @EndUserText.label: 'Status'
      @UI.identification: [ { position: 40 } ]
      @UI.lineItem: [ { position: 40, importance: #HIGH } ]
      status              : abap.char(10);    // 'SUCCESS', 'ERROR'

      @EndUserText.label: 'Result Data'
      @UI.identification: [ { position: 50 } ]
      @UI.lineItem: [ { position: 50, importance: #MEDIUM } ]
      result_data         : abap.string(0);   // JSON result

      @EndUserText.label: 'Error Message'
      @UI.identification: [ { position: 60 } ]
      @UI.lineItem: [ { position: 60, importance: #MEDIUM } ]
      error_message       : abap.string(0);

      @EndUserText.label: 'Created At'
      @UI.identification: [ { position: 70 } ]
      @UI.lineItem: [ { position: 70, importance: #LOW } ]
      created_at          : abap.dats;

      @EndUserText.label: 'Created Time'
      @UI.identification: [ { position: 80 } ]
      @UI.lineItem: [ { position: 80, importance: #LOW } ]
      created_time        : abap.tims;
}
