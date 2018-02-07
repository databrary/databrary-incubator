select * 
from volume v
where
  exists (
    select *
    from volume_access va
    where va.volume = v.id
    and va.party = -1
    and va.individual = 'PUBLIC')
order by v.id desc
