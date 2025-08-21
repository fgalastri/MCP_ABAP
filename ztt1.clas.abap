CLASS ztt1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  methods get_all_marm_records
    returning value(result_table) type ZMARM.
    
  methods sort_and_get_third_row
    importing input_table type ZMARM
    returning value(third_row) type MARM.

  " Define structure for material document with product information
  TYPES: BEGIN OF ty_material_doc_with_product,
           material_document_year TYPE c LENGTH 4,
           material_document TYPE c LENGTH 10,
           document_date TYPE dats,
           posting_date TYPE dats,
           goods_movement_code TYPE c LENGTH 2,
           created_by_user TYPE c LENGTH 12,
           creation_date TYPE dats,
           creation_time TYPE tims,
           material_document_header_text TYPE c LENGTH 25,
           product TYPE c LENGTH 40,
           product_type TYPE c LENGTH 4,
           product_group TYPE c LENGTH 9,
           base_unit TYPE c LENGTH 3,
           gross_weight TYPE p LENGTH 13 DECIMALS 3,
           net_weight TYPE p LENGTH 13 DECIMALS 3,
           weight_unit TYPE c LENGTH 3,
         END OF ty_material_doc_with_product,
         ty_mat_doc_with_prod_tab TYPE STANDARD TABLE OF ty_material_doc_with_product WITH DEFAULT KEY.

  TYPES: ty_document_year TYPE c LENGTH 4,
         ty_document_number TYPE c LENGTH 10,
         ty_product_filter TYPE c LENGTH 40.

  methods get_mat_docs_with_product
    importing
      iv_document_year TYPE ty_document_year OPTIONAL
      iv_document_number TYPE ty_document_number OPTIONAL
      iv_product TYPE ty_product_filter OPTIONAL
    returning
      value(rt_documents) TYPE ty_mat_doc_with_prod_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ztt1 IMPLEMENTATION.
  METHOD get_all_marm_records.
    " Get all records from MARM table
    SELECT * FROM marm INTO TABLE result_table.
  ENDMETHOD.
  
  METHOD sort_and_get_third_row.
    " Sort the input table and return the third row
    DATA: lt_sorted_table TYPE ZMARM.
    
    lt_sorted_table = input_table.
    
    " Sort by MATNR and MEINH
    SORT lt_sorted_table BY matnr meinh.
    
    " Get the third row if it exists
    IF lines( lt_sorted_table ) >= 3.
      third_row = lt_sorted_table[ 3 ].
    ELSE.
      CLEAR third_row.
    ENDIF.
  ENDMETHOD.

  METHOD get_mat_docs_with_product.
    " Select all material documents from I_MATERIALDOCUMENTTP 
    " and join with I_PRODUCT to get material type information
    
    DATA: lv_where TYPE string.

    " Build dynamic WHERE clause based on input parameters
    lv_where = '1 = 1'.

    " Add optional filters
    IF iv_document_year IS NOT INITIAL.
      lv_where = lv_where && ' AND md~MaterialDocumentYear = @iv_document_year'.
    ENDIF.

    IF iv_document_number IS NOT INITIAL.
      lv_where = lv_where && ' AND md~MaterialDocument = @iv_document_number'.
    ENDIF.

    IF iv_product IS NOT INITIAL.
      lv_where = lv_where && ' AND p~Product = @iv_product'.
    ENDIF.

    " Select material documents with product information using CDS views
    " Join I_MATERIALDOCUMENTTP with I_PRODUCT to get material type
    SELECT md~MaterialDocumentYear AS material_document_year,
           md~MaterialDocument AS material_document,
           md~DocumentDate AS document_date,
           md~PostingDate AS posting_date,
           md~GoodsMovementCode AS goods_movement_code,
           md~CreatedByUser AS created_by_user,
           md~CreationDate AS creation_date,
           md~CreationTime AS creation_time,
           md~MaterialDocumentHeaderText AS material_document_header_text,
           p~Product AS product,
           p~ProductType AS product_type,
           p~ProductGroup AS product_group,
           p~BaseUnit AS base_unit,
           p~GrossWeight AS gross_weight,
           p~NetWeight AS net_weight,
           p~WeightUnit AS weight_unit
      FROM I_MaterialDocumentTP AS md
      INNER JOIN I_MaterialDocumentItemTP AS mdi
        ON md~MaterialDocumentYear = mdi~MaterialDocumentYear
        AND md~MaterialDocument = mdi~MaterialDocument
      INNER JOIN I_Product AS p
        ON mdi~Material = p~Product
      INTO TABLE @rt_documents
      WHERE (lv_where)
      ORDER BY md~MaterialDocumentYear DESCENDING,
               md~MaterialDocument DESCENDING,
               p~Product ASCENDING.

  ENDMETHOD.
ENDCLASS.
