#!/usr/bin/env node

import axios from 'axios';

const config = {
  baseUrl: 'https://vhnacnc1ci.sap.naturaeco.com:44300',
  auth: { username: '375551999', password: 'Vaees@20252026!' },
  headers: { 'sap-client': '210' }
};

async function testPaths() {
  const paths = [
    // Your metadata service
    '/sap/opu/odata4/sap/zsb_mcp_metadata_service/srvd_a2x/sap/zsd_mcp_metadata_service/0001/',
    '/sap/opu/odata4/sap/zsb_mcp_metadata_service/srvd_a2x/sap/zsd_mcp_metadata_service/0001/$metadata',
    
    // Working validation service for comparison
    '/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/',
    '/sap/opu/odata4/sap/zsb_mcp_method_validate/srvd_a2x/sap/zsd_mcp_method_validate/0001/$metadata'
  ];

  for (const path of paths) {
    try {
      console.log(`🔍 Testing: ${path}`);
      const response = await axios.get(config.baseUrl + path, {
        auth: config.auth,
        headers: config.headers,
        timeout: 10000
      });
      
      console.log(`✅ ${response.status} - ${path}`);
      if (path.includes('$metadata')) {
        console.log(`📄 Content length: ${response.data?.length || 'N/A'} chars`);
      }
    } catch (error) {
      console.log(`❌ ${error.response?.status || 'ERROR'} - ${path} - ${error.message}`);
    }
    console.log('');
  }
}

testPaths().catch(console.error);
