/* eslint-env node */
/* eslint-disable no-process-env */
'use strict';

module.exports = {
  clientId: process.env.CLIENT_ID || 'd5c2fc36b20bd8be47c9',
  clientSecret: process.env.CLIENT_SECRET || '',
  port: process.env.PORT || '8080',
  url: process.env.URL || 'http://localhost:8080'
};
