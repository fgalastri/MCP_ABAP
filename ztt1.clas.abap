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
ENDCLASS.
