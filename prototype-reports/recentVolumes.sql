select av.id
       -- , p.prename, p.name (only show for admin)
       , doi
       , to_char(audit_time, 'Dy Mon DD') as audit_time
       -- , audit_user as usr (only show for admin) 
       , (select case va.share_full when true then 'PUBLIC'
                                    when false then 'PARTIAL' end
          from volume_access va
          where va.volume = av.id
            and va.party = -1
            and va.individual = 'PUBLIC'
            limit 1) as share_status
from audit.volume av
  inner join party p on av.audit_user = p.id
where av.audit_action = 'add'
order by av.audit_time desc
limit 20;  -- make limit configurable up to a maximum
