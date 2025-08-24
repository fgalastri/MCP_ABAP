#!/usr/bin/env node

/**
 * Test with correct SAP format (uppercase names, proper parameter directions)
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

class CorrectFormatTester {
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

  async testCorrectFormat() {
    console.log('🎯 Testing Correct SAP Format');
    console.log('=============================');
    
    await this.getCsrfToken();
    console.log('✅ CSRF token obtained');
    
    // Format based on SAP's own test class
    const correctClass = `CLASS LCL_ZTT1 DEFINITION.
  PUBLIC SECTION.
    METHODS TEST 
      IMPORTING field1 TYPE string
      RETURNING VALUE(field2) TYPE string.
ENDCLASS.

CLASS LCL_ZTT1 IMPLEMENTATION.
  METHOD TEST.
    CONCATENATE 'vai vai' field1 INTO field2.
  ENDMETHOD.
ENDCLASS.`;
    
    console.log('\n📝 Correct format class:');
    console.log(correctClass);
    
    try {
      const requestId = uuidv4();
      const escapedId = encodeURIComponent(`'${requestId}'`);
      const actionUrl = `${this.serviceUrl}ZC_MCP_METHOD_VALIDATE(${escapedId})/com.sap.gateway.srvd_a2x.zsd_mcp_method_validate.v0001.validate_methods`;
      
      // Correct parameter format (based on SAP test)
      const methodCall = JSON.stringify({
        method_name: "TEST",  // Uppercase!
        params: [
          {"name": "FIELD1", "direction": "EXPORTING", "type": "string", "value": "World"},  // EXPORTING, not IMPORTING
          {"name": "FIELD2", "direction": "RECEIVING", "type": "string", "value": ""}       // RECEIVING, not RETURNING
        ]
      });
      
      const payload = {
        CLASS_NAME: 'LCL_ZTT1',  // Uppercase class name
        CLASS_CODE: correctClass,
        METHOD_CALLS: methodCall
      };
      
      console.log('\n🚀 Making validation call...');
      console.log('📦 Method call:', methodCall);
      
      const response = await this.client.post(actionUrl, payload, {
        headers: { 'X-CSRF-Token': this.csrfToken }
      });
      
      console.log('\n✅ SUCCESS! Response received:');
      console.log('📊 Status:', response.data.STATUS);
      console.log('📝 Message:', response.data.MESSAGE);
      
      if (response.data.STATUS === 'SUCCESS') {
        console.log('🎉 VALIDATION SUCCESSFUL!');
        if (response.data.CALL_RESULT) {
          console.log('🎯 Call Result:', response.data.CALL_RESULT);
        }
      } else {
        console.log('⚠️ Still have an issue...');
      }
      
      console.log('\n📋 Full Response:');
      console.log(JSON.stringify(response.data, null, 2));
      
    } catch (error) {
      console.log('\n❌ FAILED:', error.message);
      if (error.response?.data) {
        console.log('📄 Error details:', JSON.stringify(error.response.data, null, 2));
      }
    }
  }
}

new CorrectFormatTester().testCorrectFormat().catch(error => {
  console.error('💥 Test failed:', error);
  process.exit(1);
});
