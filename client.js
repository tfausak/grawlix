/* eslint-env browser */
(function grawlix () {
  'use strict';

  const root = 'http://localhost:8080';
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

    document.querySelectorAll('a.def').forEach((definition) => {
      const container = document.createElement('div');
      container.className = 'subs comments';

      const id = `grawlix-${definition.textContent}`;
      container.id = id;

      const comments = commentsByDefinition[definition.textContent] || [];

      const title = document.createElement('h5');
      title.textContent = pluralize(comments.length, 'comment');
      container.appendChild(title);

      const dl = document.createElement('dl');
      container.appendChild(dl);

      comments.forEach((comment) => {
        const dt = document.createElement('dt');
        dt.innerHTML = `
          <img
            alt=""
            src="${comment.avatar}"
            style="height: 2em; vertical-align: middle"
          />
          <a
            href="https://github.com/${comment.username}"
            title="${comment.name}"
          >
            @${comment.username}
          </a>
          ${comment.when}
          ${comment.version === version ? '' : `(v${comment.version})`}
        `;
        dl.appendChild(dt);

        const dd = document.createElement('dd');
        dd.textContent = comment.content;
        dl.appendChild(dd);
      });

      const form = document.createElement('form');
      form.method = 'POST';
      form.action = `${root}/comments`;
      form.innerHTML = `
        <input
          type="hidden"
          name="token"
          value="${window.localStorage.token}"
        />
        <input type="hidden" name="package" value="${packageName}" />
        <input type="hidden" name="version" value="${version}" />
        <input type="hidden" name="module" value="${moduleName}" />
        <input
          type="hidden"
          name="definition"
          value="${definition.textContent}"
        />
        <input type="hidden" name="anchor" value="${definition.id}" />
        <textarea name="content" placeholder="Hello, world!"></textarea>
        <input type="submit">
      `;
      container.appendChild(form);

      const existingContainer = document.querySelector(`[id='${id}']`);
      const parent = definition.parentElement.parentElement;
      if (existingContainer) {
        parent.replaceChild(container, existingContainer);
      } else {
        parent.appendChild(container);
      }
    });
  };

  const query = [
    ['package', packageName],
    ['version', version],
    ['module', moduleName]
  ].map(([key, value]) => `${key}=${encodeURIComponent(value)}`).join('&');
  const url = `${root}/comments?${query}`;
  request.open('GET', url);
  request.send();
}());
