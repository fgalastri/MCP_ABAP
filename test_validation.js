#!/usr/bin/env node

// Test script to validate ABAP class directly with the SAP service
import axios from 'axios';
import { v4 as uuidv4 } from 'uuid';

// SAP Configuration
const SAP_CONFIG = {
  baseUrl: 'https://vhnacnc1ci.sap.naturaeco.com:44300/',
  username: '375551999',
  password: 'Vaees@20252026!',
  client: '210',
  servicePath: '/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/'
};

class TestSAPValidation {
  constructor() {
    this.serviceUrl = SAP_CONFIG.baseUrl + SAP_CONFIG.servicePath;
    this.csrfToken = null;
    
    // Create axios instance with basic auth
    this.client = axios.create({
      auth: {
        username: SAP_CONFIG.username,
        password: SAP_CONFIG.password
      },
      headers: {
        'sap-client': SAP_CONFIG.client,
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      }
    });
  }

  async getCsrfToken() {
    try {
      console.log('🔑 Getting CSRF token...');
      const response = await this.client.get(this.serviceUrl, {
        headers: { 'X-CSRF-Token': 'Fetch' }
      });
      
      this.csrfToken = response.headers['x-csrf-token'];
      if (this.csrfToken) {
        console.log('✅ CSRF token obtained:', this.csrfToken.substring(0, 20) + '...');
        // Update client headers with the token
        this.client.defaults.headers['X-CSRF-Token'] = this.csrfToken;
        return true;
      }
      return false;
    } catch (error) {
      console.error('❌ Failed to get CSRF token:', error.message);
      return false;
    }
  }

  async validateMethods(className, classCode, methodCalls) {
    try {
      // Ensure we have CSRF token
      if (!this.csrfToken) {
        await this.getCsrfToken();
      }

      const requestId = uuidv4();
      console.log(`🚀 Validating ${className} (Request: ${requestId})`);

      // Build action URL
      const escapedId = encodeURIComponent(`'${requestId}'`);
      const actionUrl = `${this.serviceUrl}ZC_MCP_METHOD_VALIDATE(${escapedId})/com.sap.gateway.srvd_a2x.zsd_mcp_method_validate.v0001.validate_methods`;

      // Prepare payload
      const payload = {
        CLASS_NAME: className,
        CLASS_CODE: classCode,
        METHOD_CALLS: methodCalls
      };

      console.log(`📡 Calling SAP action: ${actionUrl}`);
      console.log(`📝 Payload:`, JSON.stringify(payload, null, 2));

      // Make the validation request
      const response = await this.client.post(actionUrl, payload, {
        headers: {
          'X-CSRF-Token': this.csrfToken
        }
      });

      console.log(`📊 Response status: ${response.status}`);
      console.log(`📋 Response data:`, JSON.stringify(response.data, null, 2));
      return response.data;

    } catch (error) {
      console.error('❌ Validation error:', error.message);
      if (error.response) {
        console.error('📋 Error response:', JSON.stringify(error.response.data, null, 2));
      }
      return {
        status: 'ERROR',
        message: `Connection error: ${error.message}`
      };
    }
  }
}

// Test with a simple ABAP class
async function testValidation() {
  console.log('🧪 Starting ABAP Class Validation Test\n');
  
  const testSAP = new TestSAPValidation();
  
  // Test class code
  const testClass = `CLASS lcl_test_class DEFINITION.
  PUBLIC SECTION.
    METHODS: get_message RETURNING VALUE(rv_message) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_test_class IMPLEMENTATION.
  METHOD get_message.
    rv_message = 'Hello from ABAP!'.
  ENDMETHOD.
ENDCLASS.`;

  // Test method call
  const methodCall = JSON.stringify({
    method_name: "get_message",
    params: []
  });

  try {
    const result = await testSAP.validateMethods('LCL_TEST_CLASS', testClass, methodCall);
    
    console.log('\n🎉 Test Results:');
    console.log('================');
    if (result.STATUS === 'SUCCESS' || result.status === 'SUCCESS') {
      console.log('✅ Validation SUCCESSFUL!');
    } else {
      console.log('⚠️ Validation completed with messages');
    }
    
  } catch (error) {
    console.error('💥 Test failed:', error.message);
  }
}

// Run the test
testValidation().catch(console.error);
