insert into library_names ( content )
select distinct name
from libraries
order by name asc
