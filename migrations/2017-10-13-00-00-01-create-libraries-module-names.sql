create table libraries_module_names (
  id serial primary key,
  library_id integer references libraries (id) not null,
  module_name_id integer references module_names (id) not null,
  unique (library_id, module_name_id)
)
