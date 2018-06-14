select id, prename, name, site 
from party p 
inner join authorize_view v on (v.parent, v.child) = (0,p.id) 
where not exists (select * from volume_access where party = p.id and individual = 'ADMIN')
and not exists (select * from authorize where child = p.id and parent = 0)
and site = 'EDIT' order by id asc;
