create table tests (
  id serial primary key,
  package_id integer references packages (id) not null,
  test_name_id integer references test_names (id) not null,
  condition_id integer references conditions (id) not null,
  unique (package_id, test_name_id, condition_id)
)
