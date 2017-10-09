alter table repos
add column repo_kind_id integer references repo_kinds (id),
add column repo_type_id integer references repo_types (id)
