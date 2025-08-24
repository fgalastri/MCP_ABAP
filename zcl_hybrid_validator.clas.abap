CLASS zcl_hybrid_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_validation_result,
             success TYPE abap_bool,
             temp_class_name TYPE string,
             message TYPE string,
           END OF ty_validation_result.

    METHODS create_temp_class
      IMPORTING class_name TYPE string
                class_code TYPE string
      EXPORTING result TYPE ty_validation_result.

  PRIVATE SECTION.
    METHODS generate_unique_class_name
      EXPORTING ev_class_name TYPE string.
ENDCLASS.

CLASS zcl_hybrid_validator IMPLEMENTATION.

  METHOD create_temp_class.
    DATA: lv_temp_class_name TYPE string.
    
    generate_unique_class_name(
      IMPORTING ev_class_name = lv_temp_class_name
    ).

    result-success = abap_false.
    result-temp_class_name = lv_temp_class_name.

    TRY.
        " Step 1: Get XCO environment for local development
        DATA(lo_environment) = xco_cp_generation=>environment->dev_system( 'LOCAL' ).
        
        " Step 2: Create put operation
        DATA(lo_put_operation) = lo_environment->create_put_operation( ).
        
        " Step 3: Get class builder (correct XCO syntax with hyphen)
        DATA(lo_class_builder) = lo_put_operation->for-clas.
        
        " Step 4: Add class object (convert to proper type)
        DATA(lv_class_name_conv) = CONV sxco_ao_object_name( lv_temp_class_name ).
        DATA(lo_class_put) = lo_class_builder->add_object( lv_class_name_conv ).
        lo_class_put->set_package( '$TMP' ).
        
        " Step 5: Create form specification
        DATA(lo_specification) = lo_class_put->create_form_specification( ).
        lo_specification->set_short_description( |Temp class for { class_name }| ).
        
        " Step 6: Create minimal class structure
        " Just create the class without additional properties for now
        
        " Step 7: Execute creation
        lo_put_operation->execute( ).
        
        result-success = abap_true.
        result-message = |Temporary class { lv_temp_class_name } created successfully with XCO.|.

      CATCH cx_sy_create_data_error INTO DATA(lx_create_error).
        result-message = |CREATE DATA error: { lx_create_error->get_text( ) }|.
      CATCH cx_root INTO DATA(lx_error).
        result-message = |XCO class creation failed: { lx_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD generate_unique_class_name.
    DATA: lv_seed TYPE i.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    lv_seed = lv_timestamp MOD 999999.
    DATA(lv_random) = cl_abap_random_int=>create( seed = lv_seed min = 1000 max = 9999 )->get_next( ).
    " Use proper class name format - uppercase and max 30 chars
    ev_class_name = |ZCL_TEMP_{ lv_random }|.
    " Convert to uppercase for compatibility
    TRANSLATE ev_class_name TO UPPER CASE.
  ENDMETHOD.

ENDCLASS.