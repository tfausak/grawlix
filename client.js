/* eslint-env browser */
(function grawlix () {
  'use strict';

  let commentsByDefinition = {};
  const ok = 200;
  const pluralize = (number, word) =>
    `${number} ${word}${number === 1 ? '' : 's'}`;

  const [identifier] = document
    .querySelector('#package-header p.caption')
    .textContent
    .split(':', 1);
  const index = identifier.lastIndexOf('-');
  const packageName = identifier.substring(0, index);
  const version = identifier.substring(index + 1);

  const moduleName = document
    .querySelector('#module-header p.caption')
    .textContent;

  const request = new XMLHttpRequest();
  request.onreadystatechange = () => {
    if (request.readyState !== XMLHttpRequest.DONE) {
      return;
    }
    if (request.status !== ok) {
      return console.error(request); // eslint-disable-line no-console
    }

    commentsByDefinition = {};
    JSON.parse(request.response).forEach((comment) => {
      if (commentsByDefinition[comment.definition]) {
        commentsByDefinition[comment.definition].push(comment);
      } else {
        commentsByDefinition[comment.definition] = [comment];
      }
    });

    document.querySelectorAll('a.def').forEach((element) => {
      const definition = element.textContent;
      const anchor = element.id;
      const id = `grawlix-${definition}`;
      const existingButton = document.querySelector(`[id='${id}']`);
      const button = existingButton || document.createElement('button');
      button.id = id;
      button.dataset.anchor = anchor;
      button.dataset.definition = definition;
      const comments = commentsByDefinition[definition] || [];
      button.textContent = pluralize(comments.length, 'comment');
      if (!existingButton) {
        element.insertAdjacentElement('afterend', button);
      }
    });
  };

  const root = 'http://localhost:8080';
  const query = [
    ['package', packageName],
    ['version', version],
    ['module', moduleName]
  ].map(([key, value]) => `${key}=${encodeURIComponent(value)}`).join('&');
  const url = `${root}/comments?${query}`;
  request.open('GET', url);
  request.send();
}());
