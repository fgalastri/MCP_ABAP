CLASS zcl_test_so_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS run_sales_order_tests.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS test_get_existing_orders.
    METHODS test_retrieve_order_data
      IMPORTING iv_sales_order TYPE vbeln_va.
    METHODS test_clone_order
      IMPORTING iv_sales_order TYPE vbeln_va.
    METHODS display_order_data
      IMPORTING is_order_data TYPE zcl_sales_order_manager=>ty_sales_order_data.
ENDCLASS.

CLASS zcl_test_so_manager IMPLEMENTATION.

  METHOD run_sales_order_tests.
    DATA: lo_manager TYPE REF TO zcl_sales_order_manager,
          lt_orders TYPE vbeln_va_tab,
          ls_result TYPE zcl_sales_order_manager=>ty_operation_result.

    WRITE: / 'Sales Order Manager Test Suite',
           / '=============================='.

    CREATE OBJECT lo_manager.

    " Test 1: Get existing sales orders
    test_get_existing_orders( ).

    " Get the first sales order for testing
    lo_manager->get_existing_sales_orders(
      IMPORTING et_sales_orders = lt_orders
               es_result = ls_result
    ).

    IF ls_result-success = abap_true AND lines( lt_orders ) > 0.
      DATA(lv_test_order) = lt_orders[ 1 ].
      
      " Test 2: Retrieve order data
      test_retrieve_order_data( lv_test_order ).
      
      " Test 3: Clone order (test mode)
      test_clone_order( lv_test_order ).
    ELSE.
      WRITE: / '❌ No sales orders found for testing'.
    ENDIF.

    WRITE: / '==============================',
           / 'Tests completed!'.
  ENDMETHOD.

  METHOD test_get_existing_orders.
    DATA: lo_manager TYPE REF TO zcl_sales_order_manager,
          lt_orders TYPE vbeln_va_tab,
          ls_result TYPE zcl_sales_order_manager=>ty_operation_result.

    WRITE: / 'Test 1: Getting Existing Sales Orders'.

    CREATE OBJECT lo_manager.
    
    lo_manager->get_existing_sales_orders(
      IMPORTING et_sales_orders = lt_orders
               es_result = ls_result
    ).

    IF ls_result-success = abap_true.
      WRITE: / '✅', ls_result-message.
      LOOP AT lt_orders INTO DATA(lv_order).
        WRITE: / '   📄 Sales Order:', lv_order.
      ENDLOOP.
    ELSE.
      WRITE: / '❌', ls_result-message.
    ENDIF.
    WRITE: /.
  ENDMETHOD.

  METHOD test_retrieve_order_data.
    DATA: lo_manager TYPE REF TO zcl_sales_order_manager,
          ls_order_data TYPE zcl_sales_order_manager=>ty_sales_order_data,
          ls_result TYPE zcl_sales_order_manager=>ty_operation_result.

    WRITE: / |Test 2: Retrieving Data for Sales Order { iv_sales_order }|.

    CREATE OBJECT lo_manager.
    
    lo_manager->retrieve_sales_order_data(
      EXPORTING iv_sales_order = iv_sales_order
      IMPORTING es_order_data = ls_order_data
               es_result = ls_result
    ).

    IF ls_result-success = abap_true.
      WRITE: / '✅', ls_result-message.
      display_order_data( ls_order_data ).
    ELSE.
      WRITE: / '❌', ls_result-message.
    ENDIF.
    WRITE: /.
  ENDMETHOD.

  METHOD test_clone_order.
    DATA: lo_manager TYPE REF TO zcl_sales_order_manager,
          ls_result TYPE zcl_sales_order_manager=>ty_operation_result.

    WRITE: / |Test 3: Cloning Sales Order { iv_sales_order } (Test Mode)|.

    CREATE OBJECT lo_manager.
    
    lo_manager->clone_sales_order(
      EXPORTING iv_source_order = iv_sales_order
               iv_test_mode = abap_true  " Test mode - won't actually create
      IMPORTING es_result = ls_result
    ).

    IF ls_result-success = abap_true.
      WRITE: / '✅', ls_result-message.
      IF ls_result-sales_order IS NOT INITIAL.
        WRITE: / '   📝 New sales order would be:', ls_result-sales_order.
      ENDIF.
    ELSE.
      WRITE: / '❌', ls_result-message.
    ENDIF.

    " Display BAPI messages
    IF lines( ls_result-bapi_return ) > 0.
      WRITE: / '   📋 BAPI Messages:'.
      LOOP AT ls_result-bapi_return INTO DATA(ls_message).
        CASE ls_message-type.
          WHEN 'E'.
            WRITE: / '      ❌', ls_message-message.
          WHEN 'W'.
            WRITE: / '      ⚠️ ', ls_message-message.
          WHEN OTHERS.
            WRITE: / '      ✅', ls_message-message.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    WRITE: /.
  ENDMETHOD.

  METHOD display_order_data.
    WRITE: / '   📊 Retrieved Data Summary:'.
    
    " Header information
    WRITE: / '   📋 Header Data:'.
    WRITE: / '      Doc Type:', is_order_data-header-doc_type.
    WRITE: / '      Sales Org:', is_order_data-header-sales_org.
    WRITE: / '      Distr Chan:', is_order_data-header-distr_chan.
    WRITE: / '      Division:', is_order_data-header-division.
    WRITE: / '      Currency:', is_order_data-header-currency.
    WRITE: / '      PO Number:', is_order_data-header-purch_no_c.

    " Items information
    WRITE: / |   📦 Items ({ lines( is_order_data-items ) } items):|.
    LOOP AT is_order_data-items INTO DATA(ls_item).
      WRITE: / |      Item { ls_item-itm_number }: { ls_item-material } (Qty: { ls_item-target_qty } { ls_item-sales_unit })|.
    ENDLOOP.

    " Partners information
    WRITE: / |   👥 Partners ({ lines( is_order_data-partners ) } partners):|.
    LOOP AT is_order_data-partners INTO DATA(ls_partner).
      WRITE: / |      { ls_partner-partn_role }: { ls_partner-partn_numb }|.
    ENDLOOP.

    " Conditions information
    WRITE: / |   💰 Conditions ({ lines( is_order_data-conditions ) } conditions):|.
    LOOP AT is_order_data-conditions INTO DATA(ls_condition).
      WRITE: / |      { ls_condition-cond_type }: { ls_condition-cond_value } { ls_condition-currency }|.
    ENDLOOP.

    " Schedule lines information
    WRITE: / |   📅 Schedule Lines ({ lines( is_order_data-schedules ) } schedules):|.
    LOOP AT is_order_data-schedules INTO DATA(ls_schedule).
      WRITE: / |      Item { ls_schedule-itm_number }: { ls_schedule-req_qty } on { ls_schedule-req_date }|.
    ENDLOOP.

    " Texts information
    WRITE: / |   📝 Texts ({ lines( is_order_data-texts ) } texts):|.
    LOOP AT is_order_data-texts INTO DATA(ls_text).
      WRITE: / |      { ls_text-text_id }: { ls_text-text_line }|.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
