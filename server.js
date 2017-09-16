/* eslint-env node */
'use strict';

const express = require('express');
const morgan = require('morgan');
const request = require('request');
const statuses = require('statuses');

const CLIENT_ID = process.env.CLIENT_ID || 'd5c2fc36b20bd8be47c9';
const CLIENT_SECRET = process.env.CLIENT_SECRET || '';
const PORT = process.env.PORT || '8080';

const getHealthCheck = (_req, res) => res.json(true);

const getAuthorize = (_req, res) => res
  .redirect(`https://github.com/login/oauth/authorize?client_id=${CLIENT_ID}`);

const getCallback = (req, res, next) =>
  request({
    json: true,
    method: 'POST',
    qs: {
      client_id: CLIENT_ID, // eslint-disable-line camelcase
      client_secret: CLIENT_SECRET, // eslint-disable-line camelcase
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
      headers: { 'User-Agent': 'grawlix/0.0.0' },
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
  console.error(err);
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
  .listen(PORT, () => console.log(`Listening on port ${PORT} ...`));
