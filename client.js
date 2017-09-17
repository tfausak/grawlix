/* eslint-env browser */
(function grawlix () {
  'use strict';

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

  document.querySelectorAll('a.def').forEach((element) => {
    const definition = element.textContent;
    const anchor = element.id;
    const button = document.createElement('button');
    button.textContent = '0 comments';
    button.dataset.anchor = anchor;
    button.dataset.definition = definition;
    button.dataset.module = moduleName;
    button.dataset.package = packageName;
    button.dataset.version = version;
    element.insertAdjacentElement('afterend', button);
  });
}());
