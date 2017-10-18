create table packages_repos (
  id serial primary key,
  package_id integer references packages (id) not null,
  repo_id integer references repos (id) not null,
  unique (package_id, repo_id)
)
