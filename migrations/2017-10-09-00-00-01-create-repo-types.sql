create table repo_types (
  id serial primary key,
  content text not null unique
)
