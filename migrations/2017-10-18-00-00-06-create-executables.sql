create table executables (
  id serial primary key,
  package_id integer references packages (id) not null,
  executable_name_id integer references executable_names (id) not null,
  condition_id integer references conditions (id) not null,
  unique (package_id, executable_name_id, condition_id)
)
