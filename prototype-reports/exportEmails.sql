COPY (    
select 
  case when ailst.ai is not null then 'AI' else null end,
  ca.email,
  cp.id, 
  -- coalesce(cp.prename || ' ', '') || cp.name as child, 
  cp.prename,
  cp.name,
  (select string_agg(
             coalesce(pp.prename || ' ', '') || pp.name 
             || coalesce('@' || pp.affiliation || ' ', '')
             || (case when pa.id is null then '(site:' || au.site else '(member:' || au.member end)
             || coalesce(' to ' || to_char(au.expires, 'YYYY-MM-DD') || ')', ')')             
             , 
             ',')  
   from party pp
   left join account pa on pp.id = pa.id
   inner join authorize au on pp.id = au.parent
   where au.child = ca.id
   and au.parent != 0) as authorizers,
  cp.affiliation,
  dl.delivery
from 
  party cp 
    left join account ca on cp.id = ca.id 
      left join (select * from notify where notice = (select id from notice where name = 'Newsletter')) dl on ca.id = dl.target

  left join (select child as ai from authorize_view where parent = 0 and child > 4 and site = 'EDIT') ailst on cp.id = ailst.ai 
where 
  ca.id is not null 
order by 
  cp.affiliation asc,
  ailst.ai is not null asc
) TO STDOUT (format CSV)
