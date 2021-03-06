-- used for periodic report of unique users downloading a given volume
select 
    id
  , count(distinct 
      case when audit_user = -1 then cast(audit_ip as varchar(20)) else cast(audit_user as varchar(10)) end
      ) as usrcnt
from audit.volume 
where audit_action = 'open' 
group by id order by usrcnt desc;

-- rollup over user instead of volume; use all downloads of volume, not distinct
select 
     case when audit_user = -1 then 'IP: ' || cast(audit_ip as varchar(20)) else cast(audit_user as varchar(10)) end as usr
   , count(*) 
from audit.volume 
where audit_action = 'open' 
group by usr 
order by usr asc;
