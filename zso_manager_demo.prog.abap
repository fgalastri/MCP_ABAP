*&---------------------------------------------------------------------*
*& Report ZSO_MANAGER_DEMO
*&---------------------------------------------------------------------*
*& Sales Order Manager Demo Program
*&---------------------------------------------------------------------*
REPORT zso_manager_demo.

" Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_vbeln TYPE vbeln_va OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X',
            p_clone AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

" Text elements
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN COMMENT /1(70) TEXT-004.
SELECTION-SCREEN COMMENT /1(70) TEXT-005.
SELECTION-SCREEN COMMENT /1(70) TEXT-006.
SELECTION-SCREEN END OF BLOCK b3.

START-OF-SELECTION.

  DATA: lo_manager TYPE REF TO zcl_sales_order_manager,
        ls_order_data TYPE zcl_sales_order_manager=>ty_sales_order_data,
        ls_result TYPE zcl_sales_order_manager=>ty_operation_result.

  CREATE OBJECT lo_manager.

  " Retrieve sales order data
  WRITE: / 'Sales Order Manager Demo',
         / '========================'.

  lo_manager->retrieve_sales_order_data(
    EXPORTING iv_sales_order = p_vbeln
    IMPORTING es_order_data = ls_order_data
             es_result = ls_result
  ).

  IF ls_result-success = abap_true.
    WRITE: / '✅ Data retrieval successful!'.
    WRITE: / 'Sales Order:', p_vbeln.
    
    " Display header information
    WRITE: / '📋 Header Information:'.
    WRITE: / '   Document Type:', ls_order_data-header-doc_type.
    WRITE: / '   Sales Organization:', ls_order_data-header-sales_org.
    WRITE: / '   Distribution Channel:', ls_order_data-header-distr_chan.
    WRITE: / '   Division:', ls_order_data-header-division.
    WRITE: / '   Currency:', ls_order_data-header-currency.
    WRITE: / '   Customer PO:', ls_order_data-header-purch_no_c.

    " Display items
    WRITE: / |📦 Items ({ lines( ls_order_data-items ) } found):|.
    LOOP AT ls_order_data-items INTO DATA(ls_item).
      WRITE: / |   { ls_item-itm_number }: { ls_item-material } - Qty: { ls_item-target_qty } { ls_item-sales_unit }|.
    ENDLOOP.

    " Display partners
    WRITE: / |👥 Partners ({ lines( ls_order_data-partners ) } found):|.
    LOOP AT ls_order_data-partners INTO DATA(ls_partner).
      WRITE: / |   { ls_partner-partn_role }: { ls_partner-partn_numb }|.
    ENDLOOP.

    " If clone option is selected
    IF p_clone = 'X'.
      WRITE: / '🔄 Cloning Sales Order...'.
      
      lo_manager->clone_sales_order(
        EXPORTING iv_source_order = p_vbeln
                 iv_test_mode = p_test
        IMPORTING es_result = ls_result
      ).

      IF ls_result-success = abap_true.
        WRITE: / '✅ Clone operation successful!'.
        IF p_test = 'X'.
          WRITE: / '   (Test mode - no actual creation)'.
        ENDIF.
        WRITE: / '   New sales order:', ls_result-sales_order.
      ELSE.
        WRITE: / '❌ Clone operation failed:'.
        WRITE: / '  ', ls_result-message.
      ENDIF.

      " Display BAPI messages
      IF lines( ls_result-bapi_return ) > 0.
        WRITE: / '📋 BAPI Messages:'.
        LOOP AT ls_result-bapi_return INTO DATA(ls_message).
          CASE ls_message-type.
            WHEN 'E'.
              WRITE: / '   ❌', ls_message-message.
            WHEN 'W'.
              WRITE: / '   ⚠️ ', ls_message-message.
            WHEN OTHERS.
              WRITE: / '   ✅', ls_message-message.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ELSE.
    WRITE: / '❌ Failed to retrieve sales order data:'.
    WRITE: / '  ', ls_result-message.
  ENDIF.

END-OF-SELECTION.

" Text elements would be defined as:
" TEXT-001: Sales Order Selection
" TEXT-002: Processing Options
" TEXT-003: Instructions
" TEXT-004: Enter a sales order number to analyze its structure
" TEXT-005: Check 'Clone' to create a copy using BAPI_SALESORDER_CREATEFROMDATA
" TEXT-006: Test mode prevents actual creation - uncheck for real creation
