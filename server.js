'use strict';

const express = require('express');
const morgan = require('morgan');
const statuses = require('statuses');

const PORT = process.env.PORT || '8080';

const getHealthCheck = (_req, res) => res.json(null);

const notFound = (_req, res) => res.status(statuses('not found')).json(null);

const internalServerError = (err, _req, res, _next) => {
  console.error(err);
  res.status(statuses('internal server error')).json(null);
};

express()
  .disable('x-powered-by')
  .use(morgan('tiny'))
  .get('/health-check', getHealthCheck)
  .use(notFound)
  .use(internalServerError)
  .listen(PORT, () => console.log(`Listening on port ${PORT} ...`));
