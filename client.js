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

  const definitions = Array
    .from(document.querySelectorAll('a.def'))
    .map((element) => ({ id: element.id, name: element.textContent }));

  // eslint-disable-next-line no-console
  definitions.map((definition) => console
    .log(packageName, version, moduleName, definition.name, definition.id));
}());
