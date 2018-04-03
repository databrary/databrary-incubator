select 
  'cp /nyu/store/' || substr(cast(sha1 as varchar(80)), 3, 2) || '/' || right(cast(sha1 as varchar(80)), -4) 
  || ' /nyu/stage/ebergelson/' || volume || '/' || container || '/' || name 
from slot_asset sa 
  inner join asset a on sa.asset = a.id 
where container = 25581;
