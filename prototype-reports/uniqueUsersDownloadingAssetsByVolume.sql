select 
    volume
  , count(distinct 
         case when audit_user = -1
                 then cast(audit_ip as varchar(20)) 
                 else cast(audit_user as varchar(10)) end) as usrcnt 
from audit.slot_asset sa 
  inner join container c on sa.container = c.id 
where audit_action = 'open' 
group by volume 
order by usrcnt desc
