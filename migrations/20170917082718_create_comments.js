/* eslint-env node */
'use strict';

module.exports = {
  down: (db) => db.schema.dropTable('comments'),
  up: (db) => db.schema.createTable('comments', (table) => {
    table.increments('id');
    table.integer('user').index().notNullable().references('users.id');
    table.timestamp('when').index().defaultTo(db.fn.now());
    table.string('package').index().notNullable();
    table.string('version').index().notNullable();
    table.string('module').index().notNullable();
    table.string('definition').index().notNullable();
    table.string('anchor').notNullable();
    table.text('content').notNullable();
  })
};
