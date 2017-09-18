/* eslint-env node */
'use strict';

const bodyParser = require('body-parser');
const config = require('./config');
const cors = require('cors');
const express = require('express');
const grawlix = require('./package.json');
const knex = require('knex');
const morgan = require('morgan');
const qs = require('qs');
const request = require('request');
const statuses = require('statuses');

const db = knex(require('./knexfile'));
// eslint-disable-next-line no-console
db.on('query', (query) => console.log(`${query.sql} -- [${query.bindings}]`));

const getHealthCheck = (_req, res, next) =>
  db.select(db.raw('1'))
    .then(() => res.json(true))
    .catch((err) => next(err));

const getAuthorize = (req, res) => {
  const query = qs.stringify({
    client_id: config.clientId, // eslint-disable-line camelcase
    redirect_uri: // eslint-disable-line camelcase
      `${config.url}/callback?${qs.stringify({ comment: req.query.comment })}`
  });
  const url = `https://github.com/login/oauth/authorize?${query}`;
  res.redirect(url);
};

const getCallback = (req, res, next) =>
  request({
    json: true,
    method: 'POST',
    qs: {
      client_id: config.clientId, // eslint-disable-line camelcase
      client_secret: config.clientSecret, // eslint-disable-line camelcase
      code: req.query.code
    },
    url: 'https://github.com/login/oauth/access_token'
  }, (err, _res, body) => {
    if (err) {
      return next(err);
    }
    if (body.error) {
      return next(body);
    }

    const { access_token: token } = body;

    request({
      headers: { 'User-Agent': `${grawlix.name}/${grawlix.version}` },
      json: true,
      qs: { access_token: token }, // eslint-disable-line camelcase
      url: 'https://api.github.com/user'
    }, (err, _res, body) => {
      if (err) {
        return next(err);
      }
      if (body.error) {
        return next(body);
      }

      const { avatar_url: avatar, email, login: username, name } = body;
      db
        .transaction((trx) => trx
          .insert({ avatar, email, name, token, username })
          .into('users')
          .then(([id]) => id)
          .catch(() => trx
            .where({ name })
            .update({ avatar, email, token, username })
            .into('users'))
          .then((id) => req.query.comment
            ? trx
              .where({ id: req.query.comment })
              .update({ user: id })
              .into('comments')
              .then(() => res.json(token))
            : res.json(token)))
        .catch((err) => next(err));
    });
  });

const getClient = (_req, res) => res.sendFile('client.js', { root: '.' });

const getComments = (req, res, next) => db('comments')
  .select(
    'comments.anchor',
    'comments.content',
    'comments.definition',
    'comments.module',
    'comments.package',
    'comments.version',
    'comments.when',
    'users.avatar',
    'users.name',
    'users.username'
  )
  .innerJoin('users', 'comments.user', 'users.id')
  .where(req.query.package ? { 'comments.package': req.query.package } : true)
  .where(req.query.version ? { 'comments.version': req.query.version } : true)
  .where(req.query.module ? { 'comments.module': req.query.module } : true)
  .where(req.query.definition
    ? { 'comments.definition': req.query.definition }
    : true)
  .whereNotNull('comments.user')
  .orderBy('comments.when', 'desc')
  .then((comments) => res.json(comments))
  .catch((err) => next(err));

const postComment = (req, res, next) => db
  .transaction((trx) => trx
    .select('id')
    .from('users')
    .where({ token: req.body.token })
    .then((users) => trx
      .insert({
        anchor: req.body.anchor,
        content: req.body.content,
        definition: req.body.definition,
        module: req.body.module,
        package: req.body.package,
        user: users[0] ? users[0].id : null,
        version: req.body.version
      })
      .into('comments'))
    .then(([id]) => trx.select().from('comments').where({ id })))
  .then(([comment]) => comment.user
    ? res.json(true)
    : res.redirect(`/authorize?${qs.stringify({ comment: comment.id })}`))
  .catch((err) => next(err));

const notFound = (_req, res) => res.status(statuses('not found')).json(false);

const internalServerError = (err, _req, res, _next) => {
  console.error(err); // eslint-disable-line no-console
  res.status(statuses('internal server error')).json(false);
};

express()
  .disable('x-powered-by')
  .use(cors())
  .use(morgan('tiny'))
  .get('/health-check', getHealthCheck)
  .get('/authorize', getAuthorize)
  .get('/callback', getCallback)
  .get('/client', getClient)
  .get('/comments', getComments)
  .post('/comments', bodyParser.urlencoded({ extended: false }), postComment)
  .use(notFound)
  .use(internalServerError)
  .listen(config.port, () =>
    // eslint-disable-next-line no-console
    console.log(`Listening on port ${config.port} ...`));
