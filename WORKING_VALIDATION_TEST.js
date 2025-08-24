#!/usr/bin/env node

/**
 * ✅ WORKING ABAP VALIDATION TEST
 * 
 * This script successfully validates ABAP classes with the SAP system.
 * Use this as reference for the correct format.
 */

import axios from 'axios';
import { v4 as uuidv4 } from 'uuid';

const SAP_CONFIG = {
  baseUrl: 'https://vhnacnc1ci.sap.naturaeco.com:44300/',
  username: '375551999',
  password: 'Vaees@20252026!',
  client: '210',
  servicePath: '/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/'
};

class WorkingValidator {
  constructor() {
    this.serviceUrl = SAP_CONFIG.baseUrl + SAP_CONFIG.servicePath;
    this.csrfToken = null;
    this.cookies = [];
    
    this.client = axios.create({
      auth: { username: SAP_CONFIG.username, password: SAP_CONFIG.password },
      headers: { 'sap-client': SAP_CONFIG.client, 'Accept': 'application/json', 'Content-Type': 'application/json' },
      withCredentials: true
    });

    this.client.interceptors.request.use(request => {
      if (this.cookies.length > 0) {
        request.headers.Cookie = this.cookies.join('; ');
      }
      return request;
    });

    this.client.interceptors.response.use(response => {
      if (response.headers['set-cookie']) {
        this.cookies = response.headers['set-cookie'].map(cookie => cookie.split(';')[0]);
      }
      return response;
    }, error => error);
  }

  async getCsrfToken() {
    const response = await this.client.get(this.serviceUrl, {
      headers: { 'X-CSRF-Token': 'Fetch' }
    });
    this.csrfToken = response.headers['x-csrf-token'];
    return !!this.csrfToken;
  }

  transformClass(originalCode, className) {
    const localClassName = `LCL_${className.toUpperCase()}`;
    
    let transformedCode = originalCode
      .replace(/CLASS\s+\w+\s+DEFINITION/i, `CLASS ${localClassName} DEFINITION`)
      .replace(/CLASS\s+\w+\s+IMPLEMENTATION/i, `CLASS ${localClassName} IMPLEMENTATION`)
      // Remove PUBLIC declarations
      .replace(/\s+PUBLIC\s+FINAL\s+CREATE\s+PUBLIC\s+\./gi, '.')
      .replace(/\s+PUBLIC\s+FINAL\s+\./gi, '.')
      .replace(/\s+PUBLIC\s+CREATE\s+PUBLIC\s+\./gi, '.')
      .replace(/\s+PUBLIC\s+\./gi, '.')
      .replace(/\s+FINAL\s+CREATE\s+PUBLIC\s+\./gi, '.')
      .replace(/\s+FINAL\s+\./gi, '.')
      .replace(/\s+CREATE\s+PUBLIC\s+\./gi, '.')
      // Convert method names to uppercase
      .replace(/METHOD\s+(\w+)/gi, (match, methodName) => `METHOD ${methodName.toUpperCase()}`)
      .replace(/METHODS\s*:\s*(\w+)/gi, (match, methodName) => `METHODS ${methodName.toUpperCase()}`)
      .replace(/METHODS\s+(\w+)/gi, (match, methodName) => `METHODS ${methodName.toUpperCase()}`);

    return { localClassName, transformedCode };
  }

  createMethodCall(methodName, importingParams, returningParam) {
    const params = [];
    
    // Add EXPORTING parameters (for IMPORTING method parameters)
    importingParams.forEach(param => {
      params.push({
        name: param.name.toUpperCase(),
        direction: 'EXPORTING',
        type: param.type,
        value: param.value
      });
    });
    
    // Add RECEIVING parameter (for RETURNING method parameter)
    if (returningParam) {
      params.push({
        name: returningParam.name.toUpperCase(),
        direction: 'RECEIVING',
        type: returningParam.type,
        value: ""
      });
    }
    
    return JSON.stringify({
      method_name: methodName.toUpperCase(),
      params: params
    });
  }

  async validateClass(originalCode, className, methodName, importingParams, returningParam) {
    console.log('🎯 Validating ABAP Class');
    console.log('=======================');
    
    // Get CSRF token
    await this.getCsrfToken();
    console.log('✅ CSRF token obtained');
    
    // Transform class
    const { localClassName, transformedCode } = this.transformClass(originalCode, className);
    console.log('✅ Class transformed:', localClassName);
    
    // Create method call
    const methodCall = this.createMethodCall(methodName, importingParams, returningParam);
    console.log('✅ Method call prepared:', methodName.toUpperCase());
    
    try {
      const requestId = uuidv4();
      const escapedId = encodeURIComponent(`'${requestId}'`);
      const actionUrl = `${this.serviceUrl}ZC_MCP_METHOD_VALIDATE(${escapedId})/com.sap.gateway.srvd_a2x.zsd_mcp_method_validate.v0001.validate_methods`;
      
      const payload = {
        CLASS_NAME: localClassName,
        CLASS_CODE: transformedCode,
        METHOD_CALLS: methodCall
      };
      
      console.log('\n🚀 Making validation call...');
      
      const response = await this.client.post(actionUrl, payload, {
        headers: { 'X-CSRF-Token': this.csrfToken }
      });
      
      console.log('\n📊 VALIDATION RESULT:');
      console.log('Status:', response.data.STATUS);
      console.log('Message:', response.data.MESSAGE);
      
      if (response.data.STATUS === 'SUCCESS') {
        console.log('🎉 SUCCESS! Class validation completed!');
        if (response.data.CALL_RESULT) {
          const callResult = JSON.parse(response.data.CALL_RESULT);
          console.log('🎯 Method Result:', callResult);
        }
      }
      
      return response.data;
      
    } catch (error) {
      console.log('❌ Validation failed:', error.message);
      return null;
    }
  }
}

// Test the ztt1 class
async function testZTT1() {
  const validator = new WorkingValidator();
  
  const originalClass = `CLASS ztt1 DEFINITION
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
ENDCLASS.`;

  const result = await validator.validateClass(
    originalClass,
    'ztt1',
    'test',
    [{ name: 'field1', type: 'string', value: 'test_value' }],
    { name: 'field2', type: 'string' }
  );
  
  return result;
}

console.log('🚀 Running Working ABAP Validation Test');
console.log('=======================================');

testZTT1().then(result => {
  if (result && result.STATUS === 'SUCCESS') {
    console.log('\n✅ ZTT1 CLASS VALIDATION SUCCESSFUL!');
    process.exit(0);
  } else {
    console.log('\n❌ ZTT1 class validation failed');
    process.exit(1);
  }
}).catch(error => {
  console.error('💥 Test execution failed:', error);
  process.exit(1);
});
