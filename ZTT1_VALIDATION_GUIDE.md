# ZTT1 Class Validation Guide

This guide explains how to validate your `ZTT1` class using the ABAP MCP Server.

## Overview

The `ZTT1` class is a simple ABAP class with one method `TEST` that:
- Takes an importing parameter `FIELD1` of type `STRING`
- Returns a value `FIELD2` of type `STRING`
- Concatenates "vai vai" with the input field

## Class Structure

```abap
CLASS ztt1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  methods : test importing field1 type string
  returning value(field2) type string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ztt1 IMPLEMENTATION.
  METHOD test.
    field2 = 'vai vai' && field1.
  ENDMETHOD.
ENDCLASS.
```

## Validation Methods

### 1. Using the Python Validation Script

Run the provided `validate_ztt1.py` script:

```bash
python validate_ztt1.py
```

This script will:
- Connect to your SAP system
- Send the ZTT1 class code for validation
- Call the TEST method with sample parameters
- Display validation results

### 2. Using the MCP Server Directly

If you have an MCP client (like Claude Desktop or other MCP-compatible tools), you can use the MCP server tools:

#### Tool: `validate_abap_method`

```json
{
  "class_name": "ZTT1",
  "class_code": "CLASS ztt1 DEFINITION...",
  "method_name": "TEST",
  "parameters": [
    {
      "name": "FIELD1",
      "direction": "IMPORTING",
      "type": "STRING",
      "value": "Hello World"
    },
    {
      "name": "FIELD2", 
      "direction": "RETURNING",
      "type": "STRING",
      "value": ""
    }
  ]
}
```

### 3. Direct OData API Call

You can also call the validation service directly via HTTP:

```bash
curl -X POST \
  "https://vhnacnc1ci.sap.naturaeco.com:44300/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/ZC_MCP_METHOD_VALIDATE('request-id')/com.sap.gateway.srvd_a2x.zsd_mcp_method_validate.v0001.validate_methods" \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "sap-client: 210" \
  -u "375551999:password" \
  -d '{
    "CLASS_NAME": "ZTT1",
    "CLASS_CODE": "...",
    "METHOD_CALLS": "[{\"method_name\":\"TEST\",\"params\":[...]}]"
  }'
```

## Expected Results

### Successful Validation

When the validation succeeds, you should see:

```json
{
  "request_id": "generated-uuid",
  "status": "SUCCESS", 
  "call_result": "[{\"method_name\":\"TEST\",\"param_name\":\"FIELD2\",\"value\":\"vai vaiHello World\"}]",
  "message": "Validation completed successfully"
}
```

This means:
- ✅ Class syntax is valid
- ✅ Method signature is correct
- ✅ Method execution completed
- ✅ Return value: "vai vaiHello World" (concatenation worked)

### Common Error Scenarios

#### 1. Connection Issues
```json
{
  "status": "ERROR",
  "message": "HTTP 401: Unauthorized"
}
```
**Solution**: Check SAP credentials in environment variables

#### 2. Service Not Active
```json
{
  "status": "ERROR", 
  "message": "HTTP 404: Not Found"
}
```
**Solution**: Ensure the RAP service `ZSB_MCP_METHOD_VALIDATE` is published and active

#### 3. Syntax Errors
```json
{
  "status": "ERROR",
  "message": "ABAP syntax error: ..."
}
```
**Solution**: Fix ABAP syntax issues in the class code

## Testing Different Scenarios

### Test Case 1: Basic Validation
- Input: "World"
- Expected Output: "vai vaiWorld"

### Test Case 2: Empty String
- Input: ""
- Expected Output: "vai vai"

### Test Case 3: Special Characters
- Input: "Test!@#"
- Expected Output: "vai vaiTest!@#"

## Troubleshooting

### 1. CSRF Token Issues
If you get CSRF token errors:
- Check that the service supports CSRF token fetching
- Ensure proper headers are sent

### 2. Class Not Found
If the system says class doesn't exist:
- The validation creates classes dynamically
- Ensure your `CLASS_CODE` parameter contains the complete class definition

### 3. Type Conversion Issues
If parameter types don't match:
- Ensure parameter types in the method call match the class definition
- Use proper ABAP type names (STRING, I, P, etc.)

## Integration with Development Workflow

1. **Development**: Write/modify ABAP class locally
2. **Validation**: Use MCP server to validate before transport
3. **Testing**: Test different parameter combinations
4. **Deployment**: Transport to target system after validation

## Security Considerations

- The validation service executes ABAP code
- Only use with trusted code
- Ensure proper SAP authorizations are in place
- Consider input validation for production use

## Next Steps

1. Try running `python validate_ztt1.py` to see the validation in action
2. Experiment with different input values
3. Modify the ZTT1 class and re-validate
4. Integrate validation into your ABAP development workflow




