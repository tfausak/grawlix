/* eslint-env node */
'use strict';

module.exports = {
  down: (db) => db.schema.dropTable('users'),
  up: (db) => db.schema.createTable('users', (table) => {
    table.increments('id');
    table.string('avatar').notNullable();
    table.string('email');
    table.string('name');
    table.string('token').notNullable();
    table.string('username').notNullable().unique();
  })
};
