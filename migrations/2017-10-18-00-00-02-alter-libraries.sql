alter table libraries
add column library_name_id integer references library_names (id),
add column condition_id integer references conditions (id),
add unique (package_id, library_name_id, condition_id)
