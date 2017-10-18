create table dependencies (
  id serial primary key,
  constraint_id integer references constraints (id) not null,
  package_name_id integer references package_names (id) not null,
  unique (constraint_id, package_name_id)
)
