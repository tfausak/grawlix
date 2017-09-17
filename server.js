/* eslint-env node */
'use strict';

const config = require('./config');
const express = require('express');
const grawlix = require('./package.json');
const knex = require('knex');
const morgan = require('morgan');
const request = require('request');
const statuses = require('statuses');

const db = knex(require('./knexfile'));

const getHealthCheck = (_req, res, next) =>
  db.select(db.raw('1'))
    .then(() => res.json(true))
    .catch((err) => next(err));

const getAuthorize = (_req, res) => {
  // eslint-disable-next-line max-len
  const url = `https://github.com/login/oauth/authorize?client_id=${config.clientId}`;
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
      res.json({ avatar, email, name, username });
    });
  });

const getClient = (_req, res) => res.sendFile('client.js', { root: '.' });

const notFound = (_req, res) => res.status(statuses('not found')).json(false);

const internalServerError = (err, _req, res, _next) => {
  console.error(err); // eslint-disable-line no-console
  res.status(statuses('internal server error')).json(false);
};

express()
  .disable('x-powered-by')
  .use(morgan('tiny'))
  .get('/health-check', getHealthCheck)
  .get('/authorize', getAuthorize)
  .get('/callback', getCallback)
  .get('/client', getClient)
  .use(notFound)
  .use(internalServerError)
  .listen(config.port, () =>
    // eslint-disable-next-line no-console
    console.log(`Listening on port ${config.port} ...`));
