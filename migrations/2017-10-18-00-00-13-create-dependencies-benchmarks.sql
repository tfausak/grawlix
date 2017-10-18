create table dependencies_benchmarks (
  id serial primary key,
  dependency_id integer references dependencies (id) not null,
  benchmark_id integer references benchmarks (id) not null,
  unique (dependency_id, benchmark_id)
)
