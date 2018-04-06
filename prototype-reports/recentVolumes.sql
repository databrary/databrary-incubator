-- wanted: public vs partial vs private 

select id, left(name, 50), alias, doi, audit_time, audit_user as usr, 
       (select count(*)
       from volume_access va
       where va.volume = av.id
       and va.party = -1
       and va.individual = 'PUBLIC') as public
from audit.volume av
where av.audit_action = 'add'
order by av.audit_time desc
limit 20;
