-- converted
select 
  'cp /nyu/store/' || substr(cast(sha1 as varchar(80)), 3, 2) || '/' || right(cast(sha1 as varchar(80)), -4) 
  || ' /nyu/stage/ebergelson/' || volume || '/' || container || '/' || name 
     || '.' || (select extension[1] from format where id = a.format) 
from slot_asset sa 
  inner join asset a on sa.asset = a.id 
where container = <SLOT ID>;

-- original
select 
  'cp /nyu/store/' || substr(cast(oa.sha1 as varchar(80)), 3, 2) || '/' || right(cast(oa.sha1 as varchar(80)), -4) 
  || ' /nyu/stage/ebergelson/' || a.volume || '/' || sa.container || '/ORIG-' || oa.name
     || '.' || (select extension[1] from format where id = oa.format)
from slot_asset sa 
  inner join asset a on sa.asset = a.id  
  inner join transcode t on t.asset = a.id
  inner join asset oa on t.orig = oa.id
where sa.container = <SLOT ID>;


-- Kanishka: log into two iTerm2 windows
-- Joy: paste in "converted" portion above, copy generated lines
-- Joy: in second iTerm2 window, open nano, paste lines with file name = slotID.sh (copy into shell script) 
-- first line: set -x (to echo command, as they take time to run)
-- next line: mkdir -p [path] at top of script ... DO NOT FORGET /NYU/STAGE!!!!!!
-- bash scriptname.sh  - can take up to ten minutes to complete, watch top
-- add , a.format after || name to see extensions

-- run the converted copy before the original copy, so that the converted "wins" in any conflict
-- add , oa.format after || oa.name to see extensions

-- generate manifest of copied files by using same query with only target ouptut (or ls -1 in the output dir)


-- TODOs:
--  make output generated name include the extension
