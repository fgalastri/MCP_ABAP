#!/usr/bin/env node

/**
 * ABAP Method Validation MCP Server - Node.js Implementation
 * 
 * This MCP server connects to SAP ABAP systems to validate method calls.
 * Much more reliable than Python version for Cursor integration.
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import axios from 'axios';
import { v4 as uuidv4 } from 'uuid';

// SAP Connection Configuration
const SAP_CONFIG = {
  baseUrl: process.env.SAP_BASE_URL || 'https://vhnacnc1ci.sap.naturaeco.com:44300/',
  username: process.env.SAP_USERNAME || '375551999',
  password: process.env.SAP_PASSWORD || 'Vaees@20252026!',
  client: process.env.SAP_CLIENT || '210',
  validationServicePath: '/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/',
  metadataServicePath: '/sap/opu/odata4/sap/zsb_mcp_metadata_service/srvd_a2x/sap/zsd_mcp_metadata_service/0001/',
  programValidationServicePath: '/sap/opu/odata4/sap/zsb_mcp_program_validate/srvd_a2x/sap/zsd_mcp_program_validate/0001/'
};

// Base SAP Connection Class
class BaseSAPConnection {
  constructor(servicePath) {
    this.serviceUrl = SAP_CONFIG.baseUrl + servicePath;
    this.csrfToken = null;
    this.csrfTokenExpiry = null;
    this.cookies = [];
    this.isTokenValid = false;
    
    // Create axios instance with session management
    this.client = axios.create({
      auth: {
        username: SAP_CONFIG.username,
        password: SAP_CONFIG.password
      },
      headers: {
        'sap-client': SAP_CONFIG.client,
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      withCredentials: true // Enable session management
    });

    // Add session-aware interceptors
    this.client.interceptors.request.use(request => {
      // Include cookies for session persistence
      if (this.cookies.length > 0) {
        request.headers.Cookie = this.cookies.join('; ');
      }
      return request;
    });

    this.client.interceptors.response.use(
      response => {
        // Capture and store cookies
        if (response.headers['set-cookie']) {
          this.cookies = response.headers['set-cookie'].map(cookie => cookie.split(';')[0]);
        }
        return response;
      },
      error => error
    );
  }

  isTokenExpired() {
    if (!this.csrfToken || !this.isTokenValid) {
      return true;
    }
    // Check if token is older than 25 minutes (SAP tokens typically expire in 30 minutes)
    if (this.csrfTokenExpiry && Date.now() > this.csrfTokenExpiry) {
      return true;
    }
    return false;
  }

  async getCsrfToken() {
    // Check if current token is still valid
    if (!this.isTokenExpired()) {
      // Token is still valid, return cached token
      return { success: true, token: this.csrfToken, cached: true };
    }

    try {
      // Getting new CSRF token...
      const response = await this.client.get(this.serviceUrl, {
        headers: { 'X-CSRF-Token': 'Fetch' }
      });
      
      // Response headers received
      this.csrfToken = response.headers['x-csrf-token'];
      if (this.csrfToken) {
        // CSRF token obtained - set expiry time (25 minutes from now)
        this.csrfTokenExpiry = Date.now() + (25 * 60 * 1000);
        this.isTokenValid = true;
        return { success: true, token: this.csrfToken, cached: false };
      }
      
      // No token received
      this.isTokenValid = false;
      return { success: false, error: 'No CSRF token in response headers' };
    } catch (error) {
      // Failed to get CSRF token
      this.isTokenValid = false;
      this.csrfToken = null;
      return { 
        success: false, 
        error: `CSRF token request failed: ${error.message}`,
        status: error.response?.status,
        statusText: error.response?.statusText
      };
    }
  }

  async validateMethods(className, classCode, methodCalls) {
    // Try validation with retry logic for CSRF token issues
    return await this.validateWithRetry(className, classCode, methodCalls);
  }

  async validateWithRetry(className, classCode, methodCalls, isRetry = false) {
    try {
      // Ensure we have valid CSRF token
      const tokenResult = await this.getCsrfToken();
      if (!tokenResult.success) {
        return {
          status: 'ERROR',
          message: `CSRF Token Error: ${tokenResult.error}`,
          errorPhase: 'CSRF_TOKEN_RETRIEVAL',
          httpStatus: tokenResult.status,
          httpStatusText: tokenResult.statusText
        };
      }

      const requestId = uuidv4();
      // Validating ${className} (Request: ${requestId}) - Token: ${tokenResult.cached ? 'Cached' : 'Fresh'}

      // Build action URL
      const escapedId = encodeURIComponent(`'${requestId}'`);
      const actionUrl = `${this.serviceUrl}ZC_MCP_METHOD_VALIDATE(${escapedId})/com.sap.gateway.srvd_a2x.zsd_mcp_method_validate.v0001.validate_methods`;

      // Prepare payload
      const payload = {
        CLASS_NAME: className,
        CLASS_CODE: classCode,
        METHOD_CALLS: methodCalls
      };

      // Calling SAP action: ${actionUrl}

      // Make the validation request
      const response = await this.client.post(actionUrl, payload, {
        headers: {
          'X-CSRF-Token': this.csrfToken
        }
      });

      // Response status: ${response.status}
      return response.data;

    } catch (error) {
      // Check if it's a CSRF token error and we haven't retried yet
      if (!isRetry && error.response?.status === 403 && 
          error.response?.data?.error?.message?.includes('CSRF')) {
        
        // Invalidate current token and retry once
        this.isTokenValid = false;
        this.csrfToken = null;
        return await this.validateWithRetry(className, classCode, methodCalls, true);
      }

      // Validation error: ${error.message}
      return {
        status: 'ERROR',
        message: `Validation Request Error: ${error.message}`,
        errorPhase: 'METHOD_VALIDATION_REQUEST',
        httpStatus: error.response?.status,
        httpStatusText: error.response?.statusText
      };
    }
  }

  async getCdsSource(cdsNames) {
    try {
      // Get CSRF token if needed
      const tokenResult = await this.getCsrfToken();
      if (!tokenResult.success) {
        return {
          success: false,
          error: 'Failed to obtain CSRF token',
          details: tokenResult
        };
      }

      // Prepare ABAP code to call the metadata service
      const className = 'LCL_CDS_METADATA_CALLER';
      const cdsNamesArray = cdsNames.map(name => `'${name.toUpperCase()}'`).join(', ');
      
      const classCode = `
CLASS ${className} DEFINITION.
  PUBLIC SECTION.
    METHODS: get_cds_data
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS ${className} IMPLEMENTATION.
  METHOD get_cds_data.
    DATA: lo_service TYPE REF TO zcl_metadata_service,
          lt_cds_names TYPE zcl_metadata_service=>ddlname_tab,
          lt_sources TYPE zcl_metadata_service=>ty_cds_source_tab.
    
    CREATE OBJECT lo_service.
    
    " Add CDS names to table
    APPEND ${cdsNamesArray} TO lt_cds_names.
    
    " Get CDS sources
    lt_sources = lo_service->get_cds_source( lt_cds_names ).
    
    " Convert to JSON (simplified approach)
    LOOP AT lt_sources ASSIGNING FIELD-SYMBOL(<source>).
      result = result && '{"cds_name":"' && <source>-cds_name && '","source":"' && <source>-source && '"},'.
    ENDLOOP.
    
    IF result IS NOT INITIAL.
      result = '[' && result && ']'.
      REPLACE ALL OCCURRENCES OF '},' IN result WITH '}'.
    ELSE.
      result = '[]'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`;

      // Call the validation service to execute our metadata retrieval
      const methodCallJson = JSON.stringify({
        method_name: 'GET_CDS_DATA',
        params: []
      });

      const validationResult = await this.validateMethods(className, classCode, methodCallJson);

      if (validationResult.STATUS === 'SUCCESS' && validationResult.CALL_RESULT) {
        try {
          // Parse the JSON result from ABAP
          const jsonResult = JSON.parse(validationResult.CALL_RESULT);
          const sources = Array.isArray(jsonResult) ? jsonResult : [jsonResult];
          
          return {
            success: true,
            data: sources.filter(source => source && source.cds_name)
          };
        } catch (parseError) {
          return {
            success: false,
            error: 'Failed to parse CDS source response',
            details: validationResult.CALL_RESULT
          };
        }
      } else {
        return {
          success: false,
          error: 'ABAP metadata service call failed',
          details: validationResult
        };
      }

    } catch (error) {
      return {
        success: false,
        error: error.message,
        details: error
      };
    }
  }

  async getTableMetadata(tableNames) {
    try {
      // Get CSRF token if needed
      const tokenResult = await this.getCsrfToken();
      if (!tokenResult.success) {
        return {
          success: false,
          error: 'Failed to obtain CSRF token',
          details: tokenResult
        };
      }

      // Prepare ABAP code to call the metadata service
      const className = 'LCL_TABLE_METADATA_CALLER';
      const tableNamesArray = tableNames.map(name => `'${name.toUpperCase()}'`).join(', ');
      
      const classCode = `
CLASS ${className} DEFINITION.
  PUBLIC SECTION.
    METHODS: get_table_data
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS ${className} IMPLEMENTATION.
  METHOD get_table_data.
    DATA: lo_service TYPE REF TO zcl_metadata_service,
          lt_table_names TYPE zcl_metadata_service=>ddlname_tab,
          lt_fields TYPE zcl_metadata_service=>ty_table_field_tab.
    
    CREATE OBJECT lo_service.
    
    " Add table names to table
    APPEND ${tableNamesArray} TO lt_table_names.
    
    " Get table metadata
    lt_fields = lo_service->get_table_stru_fields( lt_table_names ).
    
    " Convert to JSON (simplified approach)
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<field>).
      result = result && '{"tab_name":"' && <field>-tab_name && '",' &&
                        '"field_name":"' && <field>-field_name && '",' &&
                        '"key_flag":"' && <field>-key_flag && '",' &&
                        '"abap_dtype":"' && <field>-abap_dtype && '",' &&
                        '"length":"' && <field>-length && '",' &&
                        '"domain_name":"' && <field>-domain_name && '",' &&
                        '"description":"' && <field>-description && '"},'.
    ENDLOOP.
    
    IF result IS NOT INITIAL.
      result = '[' && result && ']'.
      REPLACE ALL OCCURRENCES OF '},' IN result WITH '}'.
    ELSE.
      result = '[]'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`;

      // Call the validation service to execute our metadata retrieval
      const methodCallJson = JSON.stringify({
        method_name: 'GET_TABLE_DATA',
        params: []
      });

      const validationResult = await this.validateMethods(className, classCode, methodCallJson);

      if (validationResult.STATUS === 'SUCCESS' && validationResult.CALL_RESULT) {
        try {
          // Parse the JSON result from ABAP
          const jsonResult = JSON.parse(validationResult.CALL_RESULT);
          const fields = Array.isArray(jsonResult) ? jsonResult : [jsonResult];
          
          return {
            success: true,
            data: fields.filter(field => field && field.tab_name)
          };
        } catch (parseError) {
          return {
            success: false,
            error: 'Failed to parse table metadata response',
            details: validationResult.CALL_RESULT
          };
        }
      } else {
        return {
          success: false,
        error: 'ABAP metadata service call failed',
          details: validationResult
        };
      }

    } catch (error) {
      return {
        success: false,
        error: error.message,
        details: error
      };
    }
  }
}

// Validation Service Connection (for validate_abap_method)
class ValidationSAPConnection extends BaseSAPConnection {
  constructor() {
    super(SAP_CONFIG.validationServicePath);
  }
}

// Metadata Service Connection (for get_cds_source, get_table_metadata)
class MetadataSAPConnection extends BaseSAPConnection {
  constructor() {
    super(SAP_CONFIG.metadataServicePath);
  }

  // Override the metadata methods to call the metadata service directly
  async getCdsSource(cdsNames) {
    try {
      // Get CSRF token if needed
      const tokenResult = await this.getCsrfToken();
      if (!tokenResult.success) {
        return {
          success: false,
          error: 'Failed to obtain CSRF token',
          details: tokenResult
        };
      }

      // Call the metadata service custom entity
      const entityUrl = this.serviceUrl + 'ZC_MCP_METADATA_SERVICE';
      const objectNamesJson = JSON.stringify(cdsNames);
      const filters = `request_type eq 'CDS_SOURCE' and object_names eq '${objectNamesJson}'`;
      const fullUrl = `${entityUrl}?$filter=${encodeURIComponent(filters)}`;

      const response = await this.client.get(fullUrl, {
        headers: {
          'X-CSRF-Token': this.csrfToken
        }
      });

      if (response.data && response.data.value && response.data.value.length > 0) {
        const result = response.data.value[0];
        if (result.status === 'SUCCESS' && result.result_data) {
          return {
            success: true,
            data: JSON.parse(result.result_data)
          };
        } else {
          return {
            success: false,
            error: result.message || 'CDS source retrieval failed',
            details: result
          };
        }
      } else {
        return {
          success: false,
          error: 'No data returned from metadata service',
          details: response.data
        };
      }

    } catch (error) {
      return {
        success: false,
        error: error.message,
        details: error.response?.data || error
      };
    }
  }

  async getTableMetadata(tableNames) {
    try {
      // Get CSRF token if needed
      const tokenResult = await this.getCsrfToken();
      if (!tokenResult.success) {
        return {
          success: false,
          error: 'Failed to obtain CSRF token',
          details: tokenResult
        };
      }

      // Call the metadata service custom entity
      const entityUrl = this.serviceUrl + 'ZC_MCP_METADATA_SERVICE';
      const objectNamesJson = JSON.stringify(tableNames);
      const filters = `request_type eq 'TABLE_FIELDS' and object_names eq '${objectNamesJson}'`;
      const fullUrl = `${entityUrl}?$filter=${encodeURIComponent(filters)}`;

      const response = await this.client.get(fullUrl, {
        headers: {
          'X-CSRF-Token': this.csrfToken
        }
      });

      if (response.data && response.data.value && response.data.value.length > 0) {
        const result = response.data.value[0];
        if (result.status === 'SUCCESS' && result.result_data) {
          return {
            success: true,
            data: JSON.parse(result.result_data)
          };
        } else {
          return {
            success: false,
            error: result.message || 'Table metadata retrieval failed',
            details: result
          };
        }
      } else {
        return {
          success: false,
          error: 'No data returned from metadata service',
          details: response.data
        };
      }

    } catch (error) {
      return {
        success: false,
        error: error.message,
        details: error.response?.data || error
      };
    }
  }
}

// Program Validation Service Connection (for validate_abap_program)
class ProgramValidationSAPConnection extends BaseSAPConnection {
  constructor() {
    super(SAP_CONFIG.programValidationServicePath);
  }

  async validateProgramSyntax(programName, sourceCode) {
    try {
      // Get CSRF token if needed
      const tokenResult = await this.getCsrfToken();
      if (!tokenResult.success) {
        return {
          success: false,
          error: 'Failed to obtain CSRF token',
          details: tokenResult
        };
      }

      // Call the program validation service action
      const requestId = uuidv4();
      const escapedId = encodeURIComponent(`'${requestId}'`);
      const actionUrl = `${this.serviceUrl}ProgramValidation(${escapedId})/com.sap.gateway.srvd_a2x.zsd_mcp_program_validate.v0001.validate_program`;
      
      const requestBody = {
        PROGRAM_NAME: programName,
        SOURCE_CODE: sourceCode
      };

      const response = await this.client.post(actionUrl, requestBody, {
        headers: {
          'X-CSRF-Token': this.csrfToken,
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        }
      });

      return {
        success: true,
        data: response.data
      };

    } catch (error) {
      return {
        success: false,
        error: error.message,
        details: error.response?.data || error
      };
    }
  }
}

// Initialize SAP connections
const validationConnection = new ValidationSAPConnection();
const metadataConnection = new MetadataSAPConnection();
const programValidationConnection = new ProgramValidationSAPConnection();

// Create MCP server
const server = new Server(
  {
    name: 'abap-method-validator',
    version: '1.0.0',
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Helper functions
function isStructureOrTable(type) {
  // Common ABAP structure and table indicators
  const structureTypes = [
    'MARA', 'MAKT', 'VBAK', 'VBAP', 'KNA1', 'LFA1', // Standard SAP structures
    'BAPIRET2', 'BAPI_', // BAPI structures
    'ZT_', 'ZS_', 'YT_', 'YS_', // Custom table/structure prefixes
    'TT_', 'TS_', // Table type prefixes
    'ZMARM', 'MARM' // Specific types we're working with
  ];
  
  const upperType = type.toUpperCase();
  
  // Check if it's a known structure type
  if (structureTypes.some(prefix => upperType.startsWith(prefix))) {
    return true;
  }
  
  // Check if it contains table indicators
  if (upperType.includes('_TAB') || upperType.includes('_TABLE') || upperType.includes('TABLE OF')) {
    return true;
  }
  
  return false;
}

function formatStructureValue(value, type) {
  // If value is already a JSON object/array, return as string
  if (typeof value === 'object') {
    return JSON.stringify(value);
  }
  
  // If value is a string that looks like JSON, validate and return
  if (typeof value === 'string' && (value.startsWith('{') || value.startsWith('['))) {
    try {
      JSON.parse(value); // Validate JSON
      return value;
    } catch (e) {
      // If not valid JSON, wrap in quotes
      return JSON.stringify(value);
    }
  }
  
  // For simple values, return as string
  return value || "";
}

function createMethodCallJson(methodName, params) {
  // Convert parameters to SAP's expected format
  const sapParams = [];
  
  params.forEach(param => {
    if (param.direction === 'IMPORTING') {
      // IMPORTING parameters become EXPORTING (input values)
      sapParams.push({
        name: param.name.toUpperCase(),
        direction: 'EXPORTING',
        type: param.type,
        value: param.value || ""
      });
    } else if (param.direction === 'RETURNING') {
      // RETURNING parameters become RECEIVING (output values)
      sapParams.push({
        name: param.name.toUpperCase(),
        direction: 'RECEIVING', 
        type: param.type,
        value: param.value || ""
      });
    } else {
      // Other parameters (EXPORTING, CHANGING, etc.)
      sapParams.push({
        name: param.name.toUpperCase(),
        direction: mapParameterDirection(param.direction),
        type: param.type,
        value: param.value || ""
      });
    }
  });
  
  return JSON.stringify({
    method_name: methodName.toUpperCase(),
    params: sapParams
  });
}

function mapParameterDirection(direction) {
  // Map common ABAP parameter directions to SAP's expected format
  switch (direction.toUpperCase()) {
    case 'IMPORTING': return 'EXPORTING';  // Input params are EXPORTING from caller perspective
    case 'EXPORTING': return 'IMPORTING';  // Output params are IMPORTING from caller perspective  
    case 'RETURNING': return 'RECEIVING';  // Return values are RECEIVING
    case 'CHANGING': return 'CHANGING';
    default: return direction.toUpperCase();
  }
}

function transformToLocalClass(originalCode, className) {
  // Transform global class to local class format
  const localClassName = `LCL_${className.toUpperCase()}`;
  
  let transformedCode = originalCode
    .replace(/CLASS\s+\w+\s+DEFINITION/i, `CLASS ${localClassName.toUpperCase()} DEFINITION`)
    .replace(/CLASS\s+\w+\s+IMPLEMENTATION/i, `CLASS ${localClassName.toUpperCase()} IMPLEMENTATION`)
    // Remove PUBLIC declaration from local classes (SAP requirement)
    .replace(/\s+PUBLIC\s+FINAL\s+CREATE\s+PUBLIC\s+\./gi, '.')
    .replace(/\s+PUBLIC\s+FINAL\s+\./gi, '.')
    .replace(/\s+PUBLIC\s+CREATE\s+PUBLIC\s+\./gi, '.')
    .replace(/\s+PUBLIC\s+\./gi, '.')
    .replace(/\s+FINAL\s+CREATE\s+PUBLIC\s+\./gi, '.')
    .replace(/\s+FINAL\s+\./gi, '.')
    .replace(/\s+CREATE\s+PUBLIC\s+\./gi, '.')
    // Convert method names to uppercase (SAP requirement)
    .replace(/METHOD\s+(\w+)/gi, (match, methodName) => `METHOD ${methodName.toUpperCase()}`)
    .replace(/METHODS\s*:\s*(\w+)/gi, (match, methodName) => `METHODS ${methodName.toUpperCase()}`)
    .replace(/METHODS\s+(\w+)/gi, (match, methodName) => `METHODS ${methodName.toUpperCase()}`);

  return { localClassName, transformedCode };
}

// Register tools
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return {
    tools: [
      {
        name: 'validate_abap_method',
        description: 'Validate ABAP classes and optionally execute methods against SAP system. For syntax validation, only class_name and class_code are required.',
        inputSchema: {
          type: 'object',
          properties: {
            class_name: {
              type: 'string',
              description: 'Name of the ABAP class (will be converted to local format)'
            },
            class_code: {
              type: 'string',
              description: 'Complete ABAP class code'
            },
            method_name: {
              type: 'string',
              description: 'Name of the method to validate (optional - leave empty for syntax validation only)'
            },
            parameters: {
              type: 'array',
              description: 'Array of method parameters (optional - leave empty for syntax validation only)',
              items: {
                type: 'object',
                properties: {
                  name: { type: 'string', description: 'Parameter name' },
                  direction: { type: 'string', description: 'IMPORTING/EXPORTING/CHANGING/RETURNING' },
                  type: { type: 'string', description: 'ABAP data type' },
                  value: { type: 'string', description: 'Parameter value' }
                },
                required: ['name', 'direction', 'type']
              }
            }
          },
          required: ['class_name', 'class_code']
        }
      },
      {
        name: 'get_cds_source',
        description: 'Retrieve CDS view source code from SAP system. Returns the complete DDL source code for the specified CDS views.',
        inputSchema: {
          type: 'object',
          properties: {
            cds_names: {
              type: 'array',
              description: 'Array of CDS view names to retrieve source code for',
              items: {
                type: 'string',
                description: 'CDS view name (e.g., ZV_CUSTOMER_DATA)'
              }
            }
          },
          required: ['cds_names']
        }
      },
      {
        name: 'get_table_metadata',
        description: 'Retrieve table/structure field metadata from SAP data dictionary. Returns comprehensive field information including data types, lengths, domains, and descriptions.',
        inputSchema: {
          type: 'object',
          properties: {
            table_names: {
              type: 'array',
              description: 'Array of table or structure names to retrieve metadata for',
              items: {
                type: 'string',
                description: 'Table or structure name (e.g., MARA, KNA1, custom tables)'
              }
            }
          },
          required: ['table_names']
        }
      },
      {
        name: 'validate_abap_program',
        description: 'Validate ABAP program syntax using GENERATE SUBROUTINE statement. Validates program source code for syntax errors.',
        inputSchema: {
          type: 'object',
          properties: {
            program_name: {
              type: 'string',
              description: 'Name of the ABAP program to validate'
            },
            source_code: {
              type: 'string',
              description: 'Complete ABAP program source code'
            }
          },
          required: ['program_name', 'source_code']
        }
      }
    ]
  };
});

server.setRequestHandler(CallToolRequestSchema, async (request) => {
  try {
    if (request.params.name === 'validate_abap_method') {
      const { class_name, class_code, method_name = '', parameters = [] } = request.params.arguments;

      // ABAP Validation Request:
      // Original Class: ${class_name}
      // Method: ${method_name || 'SYNTAX_VALIDATION_ONLY'}
      // Parameters: ${parameters.length}

      // Transform to local class
      const { localClassName, transformedCode } = transformToLocalClass(class_code, class_name);
      // Converted to: ${localClassName}

      // Create method call JSON (empty for syntax validation)
      const methodCallJson = method_name ? createMethodCallJson(method_name, parameters) : JSON.stringify({method_name: '', params: []});

      // Call SAP validation
      const result = await validationConnection.validateMethods(
        localClassName,
        transformedCode,
        methodCallJson
      );

      // Format response
      let responseText;
      if (result.STATUS === 'SUCCESS' || result.status === 'SUCCESS') {
        responseText = `✅ **ABAP ${method_name && method_name.trim() ? 'Method Validation' : 'Syntax Validation'} Successful**

**Original Class:** ${class_name}
**Validated as:** ${localClassName}${method_name ? `\n**Method:** ${method_name}` : ''}

**SAP Response:**
\`\`\`json
${JSON.stringify(result, null, 2)}
\`\`\`

🎉 Your ABAP class syntax is valid and the method can be called successfully!`;
      } else {
        const errorPhase = result.errorPhase || 'UNKNOWN';
        const httpDetails = result.httpStatus ? ` (HTTP ${result.httpStatus}: ${result.httpStatusText || 'Unknown'})` : '';
        
        responseText = `⚠️ **ABAP ${method_name && method_name.trim() ? 'Method Validation' : 'Syntax Validation'} Result**

**Original Class:** ${class_name}
**Validated as:** ${localClassName}${method_name ? `\n**Method:** ${method_name}` : ''}
**Error Phase:** ${errorPhase}${httpDetails}

**SAP Response:**
\`\`\`json
${JSON.stringify(result, null, 2)}
\`\`\`

${errorPhase === 'CSRF_TOKEN_RETRIEVAL' ? 
  '🔑 **Error occurred during CSRF token retrieval** - Check SAP credentials and service accessibility.' :
  '📡 **Error occurred during method validation request** - CSRF token was obtained successfully, but the validation call failed.'}`;
      }

      return {
        content: [
          {
            type: 'text',
            text: responseText
          }
        ]
      };

    } else if (request.params.name === 'get_cds_source') {
      const { cds_names } = request.params.arguments;

      // CDS Source Retrieval Request for ${cds_names.length} CDS view(s)

      const result = await metadataConnection.getCdsSource(cds_names);

      let responseText;
      if (result.success) {
        const sources = result.data || [];
        responseText = `📊 **CDS Source Code Retrieved Successfully**

**CDS Views Processed:** ${cds_names.length}
**Sources Retrieved:** ${sources.length}

`;

        sources.forEach((source, index) => {
          responseText += `### **${index + 1}. ${source.cds_name}**
\`\`\`sql
${source.source || 'No source code available'}
\`\`\`

`;
        });

        if (sources.length === 0) {
          responseText += `⚠️ No CDS source code was retrieved. This could mean:
- CDS views don't exist
- No read permissions
- Views are not active

`;
        }

      } else {
        responseText = `❌ **CDS Source Retrieval Failed**

**Error:** ${result.error || 'Unknown error'}

**Attempted CDS Views:**
${cds_names.map(name => `- ${name}`).join('\n')}
`;
      }

      return {
        content: [
          {
            type: 'text',
            text: responseText
          }
        ]
      };

    } else if (request.params.name === 'get_table_metadata') {
      const { table_names } = request.params.arguments;

      // Table Metadata Retrieval Request for ${table_names.length} table(s)

      const result = await metadataConnection.getTableMetadata(table_names);

      let responseText;
      if (result.success) {
        const fields = result.data || [];
        const groupedFields = {};
        
        // Group fields by table name
        fields.forEach(field => {
          if (!groupedFields[field.tab_name]) {
            groupedFields[field.tab_name] = [];
          }
          groupedFields[field.tab_name].push(field);
        });

        responseText = `📋 **Table Metadata Retrieved Successfully**

**Tables Processed:** ${table_names.length}
**Total Fields Retrieved:** ${fields.length}

`;

        Object.keys(groupedFields).forEach(tableName => {
          const tableFields = groupedFields[tableName];
          responseText += `### **${tableName}** (${tableFields.length} fields)

| Field | Type | Length | Key | Domain | Description |
|-------|------|--------|-----|--------|-------------|
`;
          
          tableFields.forEach(field => {
            const key = field.key_flag === 'X' ? '🔑' : '';
            const type = `${field.abap_dtype}(${field.length || 0})`;
            const desc = (field.description || '').substring(0, 30);
            responseText += `| ${field.field_name} | ${type} | ${field.length || 0} | ${key} | ${field.domain_name || ''} | ${desc} |\n`;
          });

          responseText += '\n';
        });

        if (fields.length === 0) {
          responseText += `⚠️ No table metadata was retrieved. This could mean:
- Tables don't exist
- No read permissions
- Tables are not active

`;
        }

      } else {
        responseText = `❌ **Table Metadata Retrieval Failed**

**Error:** ${result.error || 'Unknown error'}

**Attempted Tables:**
${table_names.map(name => `- ${name}`).join('\n')}
`;
      }

      return {
        content: [
          {
            type: 'text',
            text: responseText
          }
        ]
      };

    } else if (request.params.name === 'validate_abap_program') {
      const { program_name, source_code } = request.params.arguments;

      // ABAP Program Validation Request:
      // Program: ${program_name}
      // Source Code Length: ${source_code.length} characters

      const result = await programValidationConnection.validateProgramSyntax(program_name, source_code);

      let responseText;
      if (result.success) {
        const validation = result.data;
        
        if (validation.status === 'SUCCESS') {
          responseText = `✅ **ABAP Program Syntax Validation Successful**

**Program Name:** ${program_name}
**Request ID:** ${validation.request_id}
**Status:** ${validation.status}

**Validation Details:**
\`\`\`json
${JSON.stringify(validation, null, 2)}
\`\`\`

🎉 Your ABAP program syntax is valid and ready for execution!`;
        } else {
          responseText = `⚠️ **ABAP Program Syntax Validation Failed**

**Program Name:** ${program_name}
**Request ID:** ${validation.request_id}
**Status:** ${validation.status}
**Error Line:** ${validation.error_line || 'Unknown'}

**Error Message:**
${validation.message || 'No error message provided'}

**Error Details:**
${validation.error_message || 'No additional error details'}

**Validation Response:**
\`\`\`json
${JSON.stringify(validation, null, 2)}
\`\`\`

🔧 Please fix the syntax errors and try again.`;
        }
      } else {
        responseText = `❌ **ABAP Program Validation Service Error**

**Program Name:** ${program_name}
**Error:** ${result.error || 'Unknown error occurred'}

**Service Response:**
\`\`\`json
${JSON.stringify(result, null, 2)}
\`\`\`

📡 **Error occurred during program validation request** - Check service connectivity and try again.`;
      }

      return {
        content: [
          {
            type: 'text',
            text: responseText
          }
        ]
      };

    } else {
      throw new Error(`Unknown tool: ${request.params.name}`);
    }
  } catch (error) {
    // Tool error
    return {
      content: [
        {
          type: 'text',
          text: `❌ Error: ${error.message}`
        }
      ],
      isError: true
    };
  }
});

// Start server
async function main() {
  // Starting ABAP Method Validation MCP Server v3 (Node.js) with dual service routing...
  
  // Test all SAP connections on startup
  const validationTokenResult = await validationConnection.getCsrfToken();
  const metadataTokenResult = await metadataConnection.getCsrfToken();
  const programValidationTokenResult = await programValidationConnection.getCsrfToken();
  // Connection test results will be available during first tool calls
  
  const transport = new StdioServerTransport();
  await server.connect(transport);
  
  // MCP Server running and ready for connections with dual service routing
}

main().catch((error) => {
  // Server error
  process.exit(1);
});

