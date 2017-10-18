alter table libraries
drop constraint libraries_package_id_name_conditions_key,
drop column name,
drop column conditions,
alter column library_name_id set not null,
alter column condition_id set not null
