create table dependencies_executables (
  id serial primary key,
  dependency_id integer references dependencies (id) not null,
  executable_id integer references executables (id) not null,
  unique (dependency_id, executable_id)
)
