update libraries
set library_name_id = ( select id from library_names where content = libraries.name ),
condition_id = ( select id from conditions where content = libraries.conditions )
