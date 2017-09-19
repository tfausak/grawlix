/* eslint-env node */
'use strict';

const config = require('./config');

module.exports = {
  client: 'pg',
  connection: config.database,
  useNullAsDefault: true
};
