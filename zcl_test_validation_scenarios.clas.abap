CLASS zcl_test_validation_scenarios DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Test class with different parameter complexity levels
    
    " Local types for testing local type handling
    TYPES: BEGIN OF ty_local_customer,
             customer_id TYPE string,
             name TYPE string,
             email TYPE string,
             active TYPE abap_bool,
           END OF ty_local_customer.
    
    TYPES: tt_local_customers TYPE TABLE OF ty_local_customer WITH DEFAULT KEY.
    
    TYPES: BEGIN OF ty_local_order,
             order_id TYPE string,
             customer_id TYPE string,
             amount TYPE p LENGTH 13 DECIMALS 2,
             currency TYPE string,
             order_date TYPE dats,
           END OF ty_local_order.
    
    TYPES: tt_local_orders TYPE TABLE OF ty_local_order WITH DEFAULT KEY.

    " Test methods with increasing complexity
    
    " Level 1: Simple string parameters
    METHODS test_simple_strings
      IMPORTING iv_input_text TYPE string
                iv_prefix TYPE string OPTIONAL
      EXPORTING ev_result TYPE string
                ev_length TYPE i.

    METHODS test_simple_calculation
      IMPORTING iv_number1 TYPE i
                iv_number2 TYPE i
                iv_operation TYPE string DEFAULT 'ADD'
      EXPORTING ev_result TYPE i
                ev_message TYPE string.

    " Level 2: Dictionary types (MARA, KNA1, etc.)
    METHODS test_material_info
      IMPORTING is_material TYPE mara
      EXPORTING ev_material_type TYPE string
                ev_description TYPE string
                ev_valid TYPE abap_bool.

    METHODS test_customer_processing
      IMPORTING is_customer TYPE kna1
                iv_check_credit TYPE abap_bool DEFAULT abap_false
      EXPORTING ev_customer_group TYPE string
                ev_credit_status TYPE string
                ev_processed TYPE abap_bool.

    " Level 3: Local types - the challenging scenario
    METHODS test_local_customer_create
      IMPORTING iv_customer_id TYPE string
                iv_name TYPE string
                iv_email TYPE string
      EXPORTING es_customer TYPE ty_local_customer
                ev_success TYPE abap_bool.

    METHODS test_local_customer_table
      IMPORTING it_customers TYPE tt_local_customers
                iv_filter_active TYPE abap_bool DEFAULT abap_true
      EXPORTING et_filtered_customers TYPE tt_local_customers
                ev_count TYPE i.

    METHODS test_complex_local_processing
      IMPORTING it_customers TYPE tt_local_customers
                it_orders TYPE tt_local_orders
                iv_min_amount TYPE p
      EXPORTING et_customer_summary TYPE tt_local_customers
                ev_total_amount TYPE p
                ev_processed_count TYPE i.

    " Mixed complexity - dictionary + local types
    METHODS test_mixed_types
      IMPORTING is_material TYPE mara
                it_local_customers TYPE tt_local_customers
                iv_price TYPE p LENGTH 13 DECIMALS 2
      EXPORTING et_enriched_customers TYPE tt_local_customers
                ev_material_customer_count TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Helper methods
    METHODS validate_email
      IMPORTING iv_email TYPE string
      RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS calculate_operation
      IMPORTING iv_num1 TYPE i
                iv_num2 TYPE i
                iv_op TYPE string
      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS zcl_test_validation_scenarios IMPLEMENTATION.

  METHOD test_simple_strings.
    " Simple string manipulation test
    DATA: lv_temp TYPE string.
    
    IF iv_prefix IS NOT INITIAL.
      lv_temp = |{ iv_prefix }: { iv_input_text }|.
    ELSE.
      lv_temp = iv_input_text.
    ENDIF.
    
    ev_result = lv_temp.
    ev_length = strlen( lv_temp ).
  ENDMETHOD.

  METHOD test_simple_calculation.
    " Simple arithmetic operations
    ev_result = calculate_operation( 
      iv_num1 = iv_number1
      iv_num2 = iv_number2  
      iv_op = iv_operation
    ).
    
    ev_message = |Operation { iv_operation } completed: { iv_number1 } and { iv_number2 } = { ev_result }|.
  ENDMETHOD.

  METHOD test_material_info.
    " Process material master data (MARA)
    ev_valid = abap_false.
    
    IF is_material-matnr IS NOT INITIAL.
      ev_valid = abap_true.
      
      " Map material type
      CASE is_material-mtart.
        WHEN 'FERT'. ev_material_type = 'Finished Product'.
        WHEN 'HALB'. ev_material_type = 'Semi-Finished Product'.
        WHEN 'ROH'.  ev_material_type = 'Raw Material'.
        WHEN 'HAWA'. ev_material_type = 'Trading Goods'.
        WHEN OTHERS. ev_material_type = 'Other Material Type'.
      ENDCASE.
      
      " Use material description if available
      ev_description = |Material { is_material-matnr } - Type: { ev_material_type }|.
    ELSE.
      ev_description = 'Invalid material - no material number provided'.
    ENDIF.
  ENDMETHOD.

  METHOD test_customer_processing.
    " Process customer master data (KNA1)
    ev_processed = abap_false.
    
    IF is_customer-kunnr IS NOT INITIAL.
      ev_processed = abap_true.
      
      " Map customer group
      CASE is_customer-ktokd.
        WHEN '0001'. ev_customer_group = 'Domestic Customer'.
        WHEN '0002'. ev_customer_group = 'Foreign Customer'.
        WHEN '0003'. ev_customer_group = 'One-Time Customer'.
        WHEN OTHERS. ev_customer_group = 'Standard Customer'.
      ENDCASE.
      
      " Check credit status if requested
      IF iv_check_credit = abap_true.
        " Simplified credit check based on customer group
        IF is_customer-ktokd = '0001' OR is_customer-ktokd = '0002'.
          ev_credit_status = 'Credit Approved'.
        ELSE.
          ev_credit_status = 'Credit Check Required'.
        ENDIF.
      ELSE.
        ev_credit_status = 'Credit Check Skipped'.
      ENDIF.
    ELSE.
      ev_customer_group = 'Invalid Customer'.
      ev_credit_status = 'No Credit Check Possible'.
    ENDIF.
  ENDMETHOD.

  METHOD test_local_customer_create.
    " Create local customer structure - tests local type handling
    ev_success = abap_false.
    
    " Validate input
    IF iv_customer_id IS INITIAL OR iv_name IS INITIAL.
      RETURN.
    ENDIF.
    
    " Validate email format
    IF iv_email IS NOT INITIAL AND validate_email( iv_email ) = abap_false.
      RETURN.
    ENDIF.
    
    " Create customer structure
    es_customer-customer_id = iv_customer_id.
    es_customer-name = iv_name.
    es_customer-email = iv_email.
    es_customer-active = abap_true.
    
    ev_success = abap_true.
  ENDMETHOD.

  METHOD test_local_customer_table.
    " Process table of local customers - tests local table type handling
    CLEAR: et_filtered_customers, ev_count.
    
    LOOP AT it_customers INTO DATA(ls_customer).
      " Apply filter
      IF iv_filter_active = abap_true.
        IF ls_customer-active = abap_true.
          APPEND ls_customer TO et_filtered_customers.
        ENDIF.
      ELSE.
        APPEND ls_customer TO et_filtered_customers.
      ENDIF.
    ENDLOOP.
    
    ev_count = lines( et_filtered_customers ).
  ENDMETHOD.

  METHOD test_complex_local_processing.
    " Complex processing with multiple local types
    DATA: lv_customer_total TYPE p LENGTH 13 DECIMALS 2.
    
    CLEAR: et_customer_summary, ev_total_amount, ev_processed_count.
    
    " Process each customer
    LOOP AT it_customers INTO DATA(ls_customer).
      lv_customer_total = 0.
      
      " Calculate total orders for this customer
      LOOP AT it_orders INTO DATA(ls_order) WHERE customer_id = ls_customer-customer_id.
        IF ls_order-amount >= iv_min_amount.
          lv_customer_total = lv_customer_total + ls_order-amount.
        ENDIF.
      ENDLOOP.
      
      " Include customer if they have qualifying orders
      IF lv_customer_total > 0.
        APPEND ls_customer TO et_customer_summary.
        ev_total_amount = ev_total_amount + lv_customer_total.
        ev_processed_count = ev_processed_count + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD test_mixed_types.
    " Mix dictionary types (MARA) with local types
    DATA: ls_enriched_customer TYPE ty_local_customer.
    
    CLEAR: et_enriched_customers, ev_material_customer_count.
    
    " Only process if material is valid
    IF is_material-matnr IS INITIAL.
      RETURN.
    ENDIF.
    
    " Enrich customers with material information
    LOOP AT it_local_customers INTO DATA(ls_customer).
      ls_enriched_customer = ls_customer.
      
      " Add material info to customer name (simplified enrichment)
      ls_enriched_customer-name = |{ ls_customer-name } (Material: { is_material-matnr })|.
      
      APPEND ls_enriched_customer TO et_enriched_customers.
    ENDLOOP.
    
    ev_material_customer_count = lines( et_enriched_customers ).
  ENDMETHOD.

  METHOD validate_email.
    " Simple email validation
    rv_valid = abap_false.
    
    IF iv_email CS '@' AND iv_email CS '.'.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_operation.
    " Simple calculator
    CASE to_upper( iv_op ).
      WHEN 'ADD' OR '+'.
        rv_result = iv_num1 + iv_num2.
      WHEN 'SUBTRACT' OR 'SUB' OR '-'.
        rv_result = iv_num1 - iv_num2.
      WHEN 'MULTIPLY' OR 'MUL' OR '*'.
        rv_result = iv_num1 * iv_num2.
      WHEN 'DIVIDE' OR 'DIV' OR '/'.
        IF iv_num2 <> 0.
          rv_result = iv_num1 / iv_num2.
        ELSE.
          rv_result = 0.
        ENDIF.
      WHEN OTHERS.
        rv_result = 0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
