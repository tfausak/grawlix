create table versions (
  id serial primary key,
  parts integer[] not null unique
)
