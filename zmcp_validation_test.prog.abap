*&---------------------------------------------------------------------*
*& Report ZMCP_VALIDATION_TEST
*&---------------------------------------------------------------------*
*& Demo program to test MCP method validation from ABAP
*&---------------------------------------------------------------------*
REPORT zmcp_validation_test.

PARAMETERS: p_demo TYPE i DEFAULT 1.

SELECTION-SCREEN COMMENT /1(60) text-001.
SELECTION-SCREEN COMMENT /1(60) text-002.
SELECTION-SCREEN COMMENT /1(60) text-003.

INITIALIZATION.
  text-001 = 'Demo 1: Direct MCP Validation Call'.
  text-002 = 'Demo 2: OData Service Simulation'.
  text-003 = 'Demo 3: HTTP Client Validation'.

START-OF-SELECTION.

  CASE p_demo.
    WHEN 1.
      " Direct validation demo
      DATA: lo_demo1 TYPE REF TO zcl_mcp_direct_call_demo.
      CREATE OBJECT lo_demo1.
      lo_demo1->run_direct_validation( ).

    WHEN 2.
      " OData demo
      DATA: lo_demo2 TYPE REF TO zcl_mcp_odata_demo.
      CREATE OBJECT lo_demo2.
      lo_demo2->run_odata_demo( ).

    WHEN 3.
      " HTTP client demo
      DATA: lo_demo3 TYPE REF TO zcl_mcp_validation_demo.
      CREATE OBJECT lo_demo3.
      lo_demo3->run_validation_demo( ).

    WHEN OTHERS.
      WRITE: / 'Invalid demo selection. Please choose 1, 2, or 3.'.
  ENDCASE.
