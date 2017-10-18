create table dependencies_libraries (
  id serial primary key,
  dependency_id integer references dependencies (id) not null,
  library_id integer references libraries (id) not null,
  unique (dependency_id, library_id)
)
