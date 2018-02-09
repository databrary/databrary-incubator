select 
  cp.id, cp.prename, cp.name, 
  pa.id, pa.prename, pa.name
from 
  authorize_inherit ai
  inner join party cp on ai.child = cp.id
  inner join 
   (select p.* 
    from party p 
    where not exists 
      (select * from account where id = p.id) 
      and p.id > 0) pa
   on ai.parent = pa.id limit 30;
