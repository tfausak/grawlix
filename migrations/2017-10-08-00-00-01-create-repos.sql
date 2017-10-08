create table repos (
  id serial primary key,
  kind text not null,
  type text not null,
  url text not null,
  unique (kind, type, url)
)
