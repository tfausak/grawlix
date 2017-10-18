insert into repo_kinds ( content )
select distinct kind
from repos
order by kind asc
