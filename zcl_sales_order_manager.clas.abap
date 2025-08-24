CLASS zcl_sales_order_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Result structures for sales order data retrieval
    TYPES: BEGIN OF ty_sales_order_data,
             " Header data
             header TYPE bapisdhead1,
             headerx TYPE bapisdhead1x,
             
             " Item data
             items TYPE bapisditem_tab,
             itemsx TYPE bapisditemx_tab,
             
             " Partner data
             partners TYPE bapisdpartner_tab,
             
             " Pricing conditions
             conditions TYPE bapisdcond_tab,
             conditionsx TYPE bapisdcondx_tab,
             
             " Schedule lines
             schedules TYPE bapisdschedule_tab,
             schedulesx TYPE bapisdschedulex_tab,
             
             " Text data
             texts TYPE bapisdtext_tab,
             
             " Configuration data
             configurations TYPE bapisdconfig_tab,
             configurationsx TYPE bapisdconfigx_tab,
             
             " Additional data for reference
             vbak_data TYPE vbak,  " Sales document header
             vbap_data TYPE vbap_tab,  " Sales document items
             vbpa_data TYPE vbpa_tab,  " Sales document partners
             
           END OF ty_sales_order_data.

    " Status structure
    TYPES: BEGIN OF ty_operation_result,
             success TYPE abap_bool,
             message TYPE string,
             sales_order TYPE vbeln_va,
             bapi_return TYPE bapiret2_tab,
           END OF ty_operation_result.

    " Main methods
    METHODS retrieve_sales_order_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING es_order_data TYPE ty_sales_order_data
                es_result TYPE ty_operation_result.

    METHODS create_sales_order_from_data
      IMPORTING is_order_data TYPE ty_sales_order_data
                iv_test_mode TYPE abap_bool DEFAULT abap_true
      EXPORTING es_result TYPE ty_operation_result.

    METHODS get_existing_sales_orders
      EXPORTING et_sales_orders TYPE vbeln_va_tab
                es_result TYPE ty_operation_result.

    METHODS clone_sales_order
      IMPORTING iv_source_order TYPE vbeln_va
                iv_test_mode TYPE abap_bool DEFAULT abap_true
      EXPORTING es_result TYPE ty_operation_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Helper methods for data retrieval
    METHODS get_header_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING es_header TYPE bapisdhead1
                es_headerx TYPE bapisdhead1x
                es_vbak TYPE vbak.

    METHODS get_item_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING et_items TYPE bapisditem_tab
                et_itemsx TYPE bapisditemx_tab
                et_vbap TYPE vbap_tab.

    METHODS get_partner_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING et_partners TYPE bapisdpartner_tab
                et_vbpa TYPE vbpa_tab.

    METHODS get_condition_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING et_conditions TYPE bapisdcond_tab
                et_conditionsx TYPE bapisdcondx_tab.

    METHODS get_schedule_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING et_schedules TYPE bapisdschedule_tab
                et_schedulesx TYPE bapisdschedulex_tab.

    METHODS get_text_data
      IMPORTING iv_sales_order TYPE vbeln_va
      EXPORTING et_texts TYPE bapisdtext_tab.

    " Helper methods for data mapping
    METHODS map_vbak_to_bapi_header
      IMPORTING is_vbak TYPE vbak
      EXPORTING es_header TYPE bapisdhead1
                es_headerx TYPE bapisdhead1x.

    METHODS map_vbap_to_bapi_items
      IMPORTING it_vbap TYPE vbap_tab
      EXPORTING et_items TYPE bapisditem_tab
                et_itemsx TYPE bapisditemx_tab.

    METHODS map_vbpa_to_bapi_partners
      IMPORTING it_vbpa TYPE vbpa_tab
      EXPORTING et_partners TYPE bapisdpartner_tab.

    " Utility methods
    METHODS log_message
      IMPORTING iv_message TYPE string
                iv_type TYPE c DEFAULT 'I'.

    METHODS clear_leading_zeros
      IMPORTING iv_input TYPE any
      RETURNING VALUE(rv_output) TYPE string.
ENDCLASS.

CLASS zcl_sales_order_manager IMPLEMENTATION.

  METHOD retrieve_sales_order_data.
    " Main method to retrieve all sales order data
    CLEAR: es_order_data, es_result.
    es_result-success = abap_false.

    " Validate input
    IF iv_sales_order IS INITIAL.
      es_result-message = 'Sales order number is required'.
      RETURN.
    ENDIF.

    TRY.
        " Get header data
        get_header_data(
          EXPORTING iv_sales_order = iv_sales_order
          IMPORTING es_header = es_order_data-header
                   es_headerx = es_order_data-headerx
                   es_vbak = es_order_data-vbak_data
        ).

        " Get item data
        get_item_data(
          EXPORTING iv_sales_order = iv_sales_order
          IMPORTING et_items = es_order_data-items
                   et_itemsx = es_order_data-itemsx
                   et_vbap = es_order_data-vbap_data
        ).

        " Get partner data
        get_partner_data(
          EXPORTING iv_sales_order = iv_sales_order
          IMPORTING et_partners = es_order_data-partners
                   et_vbpa = es_order_data-vbpa_data
        ).

        " Get condition data
        get_condition_data(
          EXPORTING iv_sales_order = iv_sales_order
          IMPORTING et_conditions = es_order_data-conditions
                   et_conditionsx = es_order_data-conditionsx
        ).

        " Get schedule data
        get_schedule_data(
          EXPORTING iv_sales_order = iv_sales_order
          IMPORTING et_schedules = es_order_data-schedules
                   et_schedulesx = es_order_data-schedulesx
        ).

        " Get text data
        get_text_data(
          EXPORTING iv_sales_order = iv_sales_order
          IMPORTING et_texts = es_order_data-texts
        ).

        es_result-success = abap_true.
        es_result-message = |Sales order { iv_sales_order } data retrieved successfully|.
        es_result-sales_order = iv_sales_order.

      CATCH cx_root INTO DATA(lx_error).
        es_result-message = |Error retrieving sales order data: { lx_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD get_header_data.
    " Retrieve header data from VBAK table
    CLEAR: es_header, es_headerx, es_vbak.

    SELECT SINGLE * FROM vbak
      INTO es_vbak
      WHERE vbeln = iv_sales_order.

    IF sy-subrc = 0.
      " Map VBAK data to BAPI header structure
      map_vbak_to_bapi_header(
        EXPORTING is_vbak = es_vbak
        IMPORTING es_header = es_header
                 es_headerx = es_headerx
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_item_data.
    " Retrieve item data from VBAP table
    CLEAR: et_items, et_itemsx, et_vbap.

    SELECT * FROM vbap
      INTO TABLE et_vbap
      WHERE vbeln = iv_sales_order
      ORDER BY posnr.

    IF sy-subrc = 0.
      " Map VBAP data to BAPI item structures
      map_vbap_to_bapi_items(
        EXPORTING it_vbap = et_vbap
        IMPORTING et_items = et_items
                 et_itemsx = et_itemsx
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_partner_data.
    " Retrieve partner data from VBPA table
    CLEAR: et_partners, et_vbpa.

    SELECT * FROM vbpa
      INTO TABLE et_vbpa
      WHERE vbeln = iv_sales_order
      ORDER BY posnr, parvw.

    IF sy-subrc = 0.
      " Map VBPA data to BAPI partner structure
      map_vbpa_to_bapi_partners(
        EXPORTING it_vbpa = et_vbpa
        IMPORTING et_partners = et_partners
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_condition_data.
    " Retrieve pricing condition data from KONV table
    CLEAR: et_conditions, et_conditionsx.

    " Get conditions for the sales order
    SELECT k~knumv, k~kposn, k~stunr, k~zaehk, k~kschl, k~kbetr, k~waers,
           k~kpein, k~kmein
      FROM konv AS k
      INNER JOIN vbak AS v ON k~knumv = v~knumv
      INTO TABLE @DATA(lt_konv)
      WHERE v~vbeln = @iv_sales_order.

    " Map to BAPI condition structure
    LOOP AT lt_konv INTO DATA(ls_konv).
      APPEND VALUE bapisdcond(
        itm_number = |{ ls_konv-kposn ALPHA = OUT }|
        cond_type = ls_konv-kschl
        cond_value = ls_konv-kbetr
        currency = ls_konv-waers
        cond_unit = ls_konv-kmein
      ) TO et_conditions.

      APPEND VALUE bapisdcondx(
        itm_number = |{ ls_konv-kposn ALPHA = OUT }|
        cond_type = 'X'
        cond_value = 'X'
        currency = 'X'
      ) TO et_conditionsx.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_schedule_data.
    " Retrieve schedule line data from VBEP table
    CLEAR: et_schedules, et_schedulesx.

    SELECT vbeln, posnr, etenr, edatu, wmeng, meins
      FROM vbep
      INTO TABLE @DATA(lt_vbep)
      WHERE vbeln = @iv_sales_order
      ORDER BY posnr, etenr.

    " Map to BAPI schedule structure
    LOOP AT lt_vbep INTO DATA(ls_vbep).
      APPEND VALUE bapisdschedule(
        itm_number = |{ ls_vbep-posnr ALPHA = OUT }|
        sched_line = |{ ls_vbep-etenr ALPHA = OUT }|
        req_date = ls_vbep-edatu
        req_qty = ls_vbep-wmeng
        sales_unit = ls_vbep-meins
      ) TO et_schedules.

      APPEND VALUE bapisdschedulex(
        itm_number = |{ ls_vbep-posnr ALPHA = OUT }|
        sched_line = |{ ls_vbep-etenr ALPHA = OUT }|
        req_date = 'X'
        req_qty = 'X'
        sales_unit = 'X'
      ) TO et_schedulesx.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_text_data.
    " Retrieve text data from STXH/STXL tables
    CLEAR et_texts.

    " This is a simplified approach - in practice you'd use READ_TEXT function
    " to get the actual text content for different text types (header texts, item texts, etc.)
    
    " Example for header text
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0001'  " Header text
        language                = sy-langu
        name                    = iv_sales_order
        object                  = 'VBBK'  " Sales document header
      TABLES
        lines                   = DATA(lt_text_lines)
      EXCEPTIONS
        id_not_found            = 1
        language_not_found      = 2
        name_not_found          = 3
        not_found               = 4
        object_not_found        = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc = 0 AND lines( lt_text_lines ) > 0.
      APPEND VALUE bapisdtext(
        text_id = '0001'
        langu = sy-langu
        text_line = lt_text_lines[ 1 ]-tdline
      ) TO et_texts.
    ENDIF.
  ENDMETHOD.

  METHOD map_vbak_to_bapi_header.
    " Map VBAK fields to BAPI header structure
    CLEAR: es_header, es_headerx.

    " Sales document type
    es_header-doc_type = is_vbak-auart.
    es_headerx-doc_type = 'X'.

    " Sales organization
    es_header-sales_org = is_vbak-vkorg.
    es_headerx-sales_org = 'X'.

    " Distribution channel
    es_header-distr_chan = is_vbak-vtweg.
    es_headerx-distr_chan = 'X'.

    " Division
    es_header-division = is_vbak-spart.
    es_headerx-division = 'X'.

    " Sales office
    es_header-sales_off = is_vbak-vkbur.
    es_headerx-sales_off = 'X'.

    " Sales group
    es_header-sales_grp = is_vbak-vkgrp.
    es_headerx-sales_grp = 'X'.

    " Currency
    es_header-currency = is_vbak-waerk.
    es_headerx-currency = 'X'.

    " Purchase order number
    es_header-purch_no_c = is_vbak-bstnk.
    es_headerx-purch_no_c = 'X'.

    " Purchase order date
    es_header-purch_date = is_vbak-bstdk.
    es_headerx-purch_date = 'X'.

    " Valid from date
    es_header-valid_from = is_vbak-guebg.
    es_headerx-valid_from = 'X'.

    " Valid to date
    es_header-valid_to = is_vbak-gueen.
    es_headerx-valid_to = 'X'.

    " Price date
    es_header-price_date = is_vbak-prsdt.
    es_headerx-price_date = 'X'.
  ENDMETHOD.

  METHOD map_vbap_to_bapi_items.
    " Map VBAP fields to BAPI item structures
    CLEAR: et_items, et_itemsx.

    LOOP AT it_vbap INTO DATA(ls_vbap).
      " Item data
      APPEND VALUE bapisditem(
        itm_number = |{ ls_vbap-posnr ALPHA = OUT }|
        material = ls_vbap-matnr
        plant = ls_vbap-werks
        target_qty = ls_vbap-kwmeng
        sales_unit = ls_vbap-vrkme
        item_categ = ls_vbap-pstyv
        hg_lv_item = ls_vbap-uepos
        target_val = ls_vbap-netwr
        net_price = ls_vbap-netpr
        price_unit = ls_vbap-prmeh
      ) TO et_items.

      " Item change indicators
      APPEND VALUE bapisditemx(
        itm_number = |{ ls_vbap-posnr ALPHA = OUT }|
        material = 'X'
        plant = 'X'
        target_qty = 'X'
        sales_unit = 'X'
        item_categ = 'X'
        target_val = 'X'
        net_price = 'X'
        price_unit = 'X'
      ) TO et_itemsx.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_vbpa_to_bapi_partners.
    " Map VBPA fields to BAPI partner structure
    CLEAR et_partners.

    LOOP AT it_vbpa INTO DATA(ls_vbpa).
      APPEND VALUE bapisdpartner(
        itm_number = |{ ls_vbpa-posnr ALPHA = OUT }|
        partn_role = ls_vbpa-parvw
        partn_numb = ls_vbpa-kunnr
      ) TO et_partners.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_sales_order_from_data.
    " Create new sales order using BAPI with retrieved data
    CLEAR es_result.
    es_result-success = abap_false.

    DATA: lv_sales_order TYPE vbeln_va,
          lt_return TYPE bapiret2_tab.

    TRY.
        " Call BAPI to create sales order
        CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDATA'
          EXPORTING
            order_header_in     = is_order_data-header
            order_header_inx    = is_order_data-headerx
            testrun             = iv_test_mode
          IMPORTING
            salesdocument       = lv_sales_order
          TABLES
            return              = lt_return
            order_items_in      = is_order_data-items
            order_items_inx     = is_order_data-itemsx
            order_partners      = is_order_data-partners
            order_schedules_in  = is_order_data-schedules
            order_schedules_inx = is_order_data-schedulesx
            order_conditions_in = is_order_data-conditions
            order_conditions_inx = is_order_data-conditionsx
            order_text          = is_order_data-texts
            order_cfgs_ref      = is_order_data-configurations
            order_cfgs_inst     = is_order_data-configurations
            order_cfgs_part_of  = is_order_data-configurations
            order_cfgs_value    = is_order_data-configurations
            order_cfgs_blob     = is_order_data-configurations
            order_cfgs_vk       = is_order_data-configurationsx.

        " Check if there are any errors
        READ TABLE lt_return INTO DATA(ls_error) WITH KEY type = 'E'.
        IF sy-subrc = 0.
          es_result-message = ls_error-message.
          es_result-bapi_return = lt_return.
          RETURN.
        ENDIF.

        " Commit if not in test mode
        IF iv_test_mode = abap_false.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.

        es_result-success = abap_true.
        es_result-sales_order = lv_sales_order.
        es_result-bapi_return = lt_return.
        
        IF iv_test_mode = abap_true.
          es_result-message = |Sales order would be created: { lv_sales_order } (Test mode)|.
        ELSE.
          es_result-message = |Sales order created successfully: { lv_sales_order }|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        es_result-message = |Error creating sales order: { lx_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD get_existing_sales_orders.
    " Get list of existing sales orders
    CLEAR: et_sales_orders, es_result.
    
    SELECT vbeln FROM vbak
      INTO TABLE et_sales_orders
      ORDER BY vbeln.

    IF sy-subrc = 0.
      es_result-success = abap_true.
      es_result-message = |Found { lines( et_sales_orders ) } sales orders in system|.
    ELSE.
      es_result-success = abap_false.
      es_result-message = 'No sales orders found in system'.
    ENDIF.
  ENDMETHOD.

  METHOD clone_sales_order.
    " Clone an existing sales order
    DATA: ls_order_data TYPE ty_sales_order_data,
          ls_retrieve_result TYPE ty_operation_result.

    CLEAR es_result.

    " First retrieve the source order data
    retrieve_sales_order_data(
      EXPORTING iv_sales_order = iv_source_order
      IMPORTING es_order_data = ls_order_data
               es_result = ls_retrieve_result
    ).

    IF ls_retrieve_result-success = abap_false.
      es_result = ls_retrieve_result.
      RETURN.
    ENDIF.

    " Modify some fields to avoid conflicts
    CLEAR: ls_order_data-header-purch_no_c,  " Clear purchase order reference
           ls_order_data-headerx-purch_no_c.

    " Create new sales order from retrieved data
    create_sales_order_from_data(
      EXPORTING is_order_data = ls_order_data
               iv_test_mode = iv_test_mode
      IMPORTING es_result = es_result
    ).

    IF es_result-success = abap_true.
      es_result-message = |Sales order cloned from { iv_source_order }: { es_result-message }|.
    ENDIF.
  ENDMETHOD.

  METHOD log_message.
    " Simple logging method
    CASE iv_type.
      WHEN 'E'.
        WRITE: / '❌', iv_message.
      WHEN 'W'.
        WRITE: / '⚠️ ', iv_message.
      WHEN OTHERS.
        WRITE: / '✅', iv_message.
    ENDCASE.
  ENDMETHOD.

  METHOD clear_leading_zeros.
    " Remove leading zeros from numbers
    rv_output = |{ iv_input ALPHA = OUT }|.
  ENDMETHOD.

ENDCLASS.
