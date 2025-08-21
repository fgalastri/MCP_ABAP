@AbapCatalog.viewEnhancementCategory: [ #NONE ]

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Units of Measure conversion'

@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #S, dataClass: #MIXED }

define view entity ZFG_TESTCDS
  as select distinct from I_ProductUnitsOfMeasure as UOM

    inner join            I_ChangeDocument        as ChangeDoc
      on  UOM.Product                    = ChangeDoc.ChangeDocObject
      and ChangeDoc.ChangeDocObjectClass = 'MATERIAL'

    inner join            I_ChangeDocumentItem    as ChangeDocItem
      on  ChangeDoc.ChangeDocObjectClass = ChangeDocItem.ChangeDocObjectClass
      and ChangeDoc.ChangeDocObject      = ChangeDocItem.ChangeDocObject
      and ChangeDoc.ChangeDocument       = ChangeDocItem.ChangeDocument

{
  key UOM.Product               as Item,
  key UOM.AlternativeUnit       as UOM,

      cast(
        case
          when UOM.QuantityDenominator <> 0
          then cast(UOM.QuantityNumerator as abap.decfloat16) / cast(UOM.QuantityDenominator as abap.decfloat16)
          else cast(0 as abap.decfloat16)
        end as abap.decfloat16
      )                         as ERPConversionRate,

      cast('1' as abap.char(1)) as IsValid,
      ChangeDoc.CreationDate as lastChangeDate
}
