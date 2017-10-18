insert into repo_types ( content )
select distinct type
from repos
order by type asc
