select id, left(name, 50), alias, doi, audit_time, audit_user, 
       (select count(*)
       from volume_access va
       where va.volume = av.id
       and va.party = -1
       and va.individual = 'PUBLIC') as public_can_access    
from audit.volume av
where av.audit_action = 'add'
order by av.audit_time desc
limit 20;
