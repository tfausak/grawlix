create table repo_kinds (
  id serial primary key,
  content text not null unique
)
