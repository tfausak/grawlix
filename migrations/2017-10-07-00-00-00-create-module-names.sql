create table module_names (
  id serial primary key,
  content text[] not null unique
)
