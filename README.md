# MCP ABAP

Model Context Protocol (MCP) server for ABAP development with SAP integration.

## Features

### 🔧 ABAP Validation Service
- **Syntax Validation**: Validate ABAP class syntax before deployment
- **Method Execution**: Test ABAP methods with parameters and validate results
- **Local Class Conversion**: Automatically converts global ABAP classes to local format for validation
- **Error Reporting**: Detailed error messages with line numbers for quick debugging

### 📊 Metadata Service  
- **CDS Source Retrieval**: Get complete DDL source code for CDS views
- **Table Metadata**: Retrieve comprehensive field information for database tables and structures
- **Data Dictionary Integration**: Access field types, lengths, domains, and descriptions

## Architecture

### Dual Service Routing
The server automatically routes requests to the appropriate SAP OData service:
- **Validation requests** → `zsb_mcp_method_validate` service
- **Metadata requests** → `zsb_mcp_metadata_service` service

### Key Components

#### Node.js MCP Server (`server.js`)
- **BaseSAPConnection**: Shared logic for CSRF tokens and session management
- **ValidationSAPConnection**: Handles ABAP syntax validation and method execution
- **MetadataSAPConnection**: Manages CDS and table metadata retrieval
- **Automatic Class Transformation**: Converts global classes to local format
- **CSRF Token Caching**: Optimized performance with token reuse

#### ABAP Backend Classes
- **`zmcp_method_validate`**: Core validation logic for ABAP classes and methods
- **`zcl_metadata_service`**: Metadata retrieval from SAP Data Dictionary
- **`zcl_ce_mcp_meta_svc`**: RAP query provider for metadata custom entity
- **Custom Entities**: `ZC_MCP_METHOD_VALIDATE` and `ZC_MCP_METADATA_SERVICE`

## Installation

1. **Install Node.js dependencies:**
   ```bash
   npm install
   ```

2. **Configure MCP client** (add to your MCP configuration):
   ```json
   {
     "mcpServers": {
       "abap-validator": {
         "command": "node",
         "args": ["C:\\path\\to\\your\\server.js"]
       }
     }
   }
   ```

3. **Deploy ABAP components** to your SAP system:
   - Import all `.clas.abap`, `.ddls.asddls`, and `.bdef.asbdef` files
   - Activate the OData services in SAP

## Usage

### Validate ABAP Class Syntax
```javascript
// Automatically validates syntax after code generation
// Converts global class to local format
// Reports detailed error messages with line numbers
```

### Retrieve CDS Source Code
```javascript
// Get complete DDL definition for CDS views
// Includes annotations, associations, and field mappings
// Formatted source code ready for analysis
```

### Get Table Metadata
```javascript
// Comprehensive field information
// Data types, lengths, domains, descriptions
// Key field indicators and relationships
```

## Configuration

### Environment Variables
- `SAP_BASE_URL`: SAP system base URL
- `SAP_USERNAME`: SAP username
- `SAP_PASSWORD`: SAP password  
- `SAP_CLIENT`: SAP client number

### SAP Service Endpoints
- **Validation**: `/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/`
- **Metadata**: `/sap/opu/odata4/sap/zsb_mcp_metadata_service/srvd_a2x/sap/zsd_mcp_metadata_service/0001/`

## ABAP Files Structure

```
├── Core Classes
│   ├── zmcp_method_validate.clas.abap      # Main validation class
│   ├── zcl_metadata_service.clas.abap      # Metadata service class
│   └── zcl_ce_mcp_meta_svc.clas.abap      # Query provider
├── Custom Entities
│   ├── zc_mcp_method_validate.ddls.asddls  # Validation entity
│   └── zc_mcp_metadata_service.ddls.asddls # Metadata entity
├── Behavior Definitions
│   ├── zc_mcp_method_validate.bdef.asbdef  # Validation behavior
│   └── zc_mcp_metadata_service.bdef.asbdef # Metadata behavior
└── Test Classes
    ├── ztt1.clas.abap                      # Test class 1
    └── ztt2.clas.abap                      # Test class 2
```

## Features in Detail

### Automatic Error Fixing
- Detects and fixes common ABAP syntax errors
- Updates source files with corrections
- Re-validates after fixes

### Session Management
- Maintains SAP session cookies
- Handles CSRF token lifecycle
- Automatic retry on authentication failures

### Type Safety
- Validates ABAP data types and structures
- Ensures parameter type compatibility
- Comprehensive type checking

## Contributing

This project integrates with SAP S/4HANA systems and requires proper SAP development environment setup.

## License

MIT License - See LICENSE file for details.
