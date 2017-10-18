create table dependencies_tests (
  id serial primary key,
  dependency_id integer references dependencies (id) not null,
  test_id integer references tests (id) not null,
  unique (dependency_id, test_id)
)
