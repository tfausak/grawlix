create table libraries (
  id serial primary key,
  package_id integer references packages (id) not null,
  name text not null,
  conditions text not null,
  unique (name, conditions)
)
