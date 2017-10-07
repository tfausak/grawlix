create table categories_packages (
  id serial primary key,
  category_id integer references categories (id) not null,
  package_id integer references packages (id) not null,
  unique (category_id, package_id)
)
