/* eslint-env node */
/* eslint-disable no-process-env */
'use strict';

module.exports = {
  clientId: process.env.CLIENT_ID || 'd5c2fc36b20bd8be47c9',
  clientSecret: process.env.CLIENT_SECRET || '',
  database: process.env.DATABASE_URL ||
    'postgres://postgres:password@localhost:5432/postgres',
  port: process.env.PORT || '8080'
};

module.exports.url = process.env.URL ||
  `http://127.0.0.1:${module.exports.port}`;
