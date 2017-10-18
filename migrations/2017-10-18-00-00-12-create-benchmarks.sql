create table benchmarks (
  id serial primary key,
  package_id integer references packages (id) not null,
  benchmark_name_id integer references benchmark_names (id) not null,
  condition_id integer references conditions (id) not null,
  unique (package_id, benchmark_name_id, condition_id)
)
