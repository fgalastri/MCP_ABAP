CLASS zcl_metadata_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_cds_source,
             cds_name        TYPE ddlname,
             source type string,
           END OF ty_cds_source,
           ty_cds_source_tab TYPE STANDARD TABLE OF ty_cds_source WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_table_field,
             tab_name     TYPE tabname,
             field_name   TYPE fieldname,
             key_flag     TYPE keyflag,
             rollname     TYPE rollname,
             domain_name  TYPE domname,
             abap_dtype   TYPE dd03l-inttype,
             db_dtype     TYPE dd03l-datatype,
             length       TYPE ddleng,
             decimals     TYPE decimals,
             description  TYPE as4text,
           END OF ty_table_field,
           ty_table_field_tab TYPE STANDARD TABLE OF ty_table_field WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_method_param,
             cls_name      TYPE seoclsname,
             method_name   TYPE seocpdname,
             param_name    TYPE string,
             param_kind    TYPE c LENGTH 1,    "I, E, C, R, T (import, export, changing, return, tables)
             abap_dtype    TYPE string,
             length        TYPE i,
             decimals      TYPE i,
           END OF ty_method_param,
           ty_method_param_tab TYPE STANDARD TABLE OF ty_method_param WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_function_param,
             func_name     TYPE rs38l_fnam,
             param_name    TYPE rs38l_par_,
             param_type    TYPE rs38l_kind,        "I/E/C/T/R: import/export/changing/tables/exceptions
             structure     TYPE rs38l_typ,
             default_val   TYPE rs38l_defo,
             reference     TYPE rs38l_refe,
             optional      TYPE rs38l_opti,
           END OF ty_function_param,
           ty_function_param_tab TYPE STANDARD TABLE OF ty_function_param WITH DEFAULT KEY.

types ddlname_tab TYPE STANDARD TABLE OF  sxco_cds_object_name.

    " Métodos públicos
    METHODS:
      get_cds_source
        IMPORTING
          it_cds_names TYPE ddlname_tab
        RETURNING
          VALUE(cds_source) TYPE ty_cds_source_tab,

      get_table_stru_fields
        IMPORTING
          it_tabnames TYPE ddlname_tab
        RETURNING
          VALUE(rt_fields) TYPE ty_table_field_tab,

      get_classes_methods
        IMPORTING
          it_class_names TYPE ddlname_tab
        RETURNING
          VALUE(rt_params) TYPE ty_method_param_tab,

      get_functions
        IMPORTING
          it_function_names TYPE ddlname_tab
        RETURNING
          VALUE(rt_params) TYPE ty_function_param_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_metadata_service IMPLEMENTATION.
  METHOD get_cds_source.
    DATA source TYPE ty_cds_source.

    LOOP AT it_cds_names ASSIGNING FIELD-SYMBOL(<ls_cds>).
      CLEAR source.
      source-cds_name = <ls_cds>.

      TRY.
          cl_dd_ddl_handler_factory=>create( )->read(
            EXPORTING name         = CONV ddlname( to_upper( source-cds_name ) )
            IMPORTING ddddlsrcv_wa = DATA(ddddlsrcv_wa) ).

          source-source = ddddlsrcv_wa-source.
          APPEND source TO cds_source.
        CATCH cx_dd_ddl_read INTO DATA(exc).
          cl_demo_output=>display( exc->get_text( ) ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_table_stru_fields.
    rt_fields = VALUE #( ).
    LOOP AT it_tabnames ASSIGNING FIELD-SYMBOL(<l_tab>).
      DATA(lv_tab) = <l_tab>.
      SELECT a~fieldname,
             a~keyflag,
             a~rollname,
             a~inttype,
             a~datatype,
             a~leng,
             a~decimals,
             b~domname,
             txt~ddtext  AS description
        INTO TABLE @DATA(lt_fields)
        FROM dd03l AS a
               LEFT OUTER JOIN
                 dd04l AS b ON a~rollname = b~rollname
                   LEFT OUTER JOIN
                     dd03t AS txt ON  txt~tabname    = a~tabname
                                  AND txt~fieldname  = a~fieldname
                                  AND txt~ddlanguage = @sy-langu
        WHERE a~tabname  = @lv_tab
          AND a~as4local = 'A'.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<lf>).
        rt_fields = VALUE #( BASE rt_fields
                             ( tab_name    = lv_tab
                               field_name  = <lf>-fieldname
                               key_flag    = <lf>-keyflag
                               rollname    = <lf>-rollname
                               domain_name = <lf>-domname
                               abap_dtype  = <lf>-inttype
                               db_dtype    = <lf>-datatype
                               length      = <lf>-leng
                               decimals    = <lf>-decimals
                               description = <lf>-description ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_classes_methods.


* DATA(lo_class) = xco_cp_abap=>class( co_class_name ).
*
*    " DEFINITION.
*    DATA(ls_definition_content) = lo_class->definition->content( )->get( ).
*    out->write( |Definition of { lo_class->name }:| ).
*    out->write( ls_definition_content ).
*
*    " PUBLIC class methods.
*    DATA(lt_public_methods) = lo_class->definition->section-public->components->class_method->all->get( ).

  ENDMETHOD.


  METHOD get_functions.
    rt_params = VALUE #( ).
    LOOP AT it_function_names ASSIGNING FIELD-SYMBOL(<func>).
      SELECT funcname,
             parameter,
             paramtype,
             structure,
             defaultval,
             reference,
             optional
        INTO TABLE @DATA(lt_params)
        FROM fupararef
        WHERE funcname = @<func>
          AND r3state  = 'A'.             "Somente funções ativas

      LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<p>).
        rt_params = VALUE #( BASE rt_params
                             ( func_name   = <p>-funcname
                               param_name  = <p>-parameter
                               param_type  = <p>-paramtype
                               structure   = <p>-structure
                               default_val = <p>-defaultval
                               reference   = <p>-reference
                               optional    = <p>-optional ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.