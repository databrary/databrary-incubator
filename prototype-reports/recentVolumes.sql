select id
       , left(name, 50) , alias, doi
       , to_char(audit_time, 'Mon DD YYYY HH12:MI AM') as audit_time, audit_user as usr, 
       (select case va.share_full when true then 'PUBLIC'
                                  when false then 'PARTIAL' end
       from volume_access va
       where va.volume = av.id
       and va.party = -1
       and va.individual = 'PUBLIC'
       limit 1) as share_status
from audit.volume av
where av.audit_action = 'add'
order by av.audit_time desc
limit 20;
