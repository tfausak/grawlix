create table packages (
  id serial primary key,
  package_name_id integer references package_names (id) not null,
  version_id integer references versions (id) not null,
  revision integer not null,
  license_id integer references licenses (id) not null,
  synopsis text not null,
  description text not null,
  url text not null,
  unique (package_name_id, version_id, revision)
)
