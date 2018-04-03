select 
  'cp /nyu/store/' || substr(cast(sha1 as varchar(80)), 3, 2) || '/' || right(cast(sha1 as varchar(80)), -4) 
  || ' /nyu/stage/ebergelson/' || volume || '/' || container || '/' || name 
from slot_asset sa 
  inner join asset a on sa.asset = a.id 
where container = 25581;

-- copy into shell script
-- add mkdir at top of script
-- bash scriptname.sh  - can take up to ten minutes to complete, watch top

-- generate manifest of copied files by using same query with only target ouptut
