alter table libraries
drop constraint libraries_name_conditions_key,
add unique (package_id, name, conditions)
