-- converted and conversion not needed
---- this will retrieve all converted assets combined with all assets that didn't need any conversion
select 
  'cp /nyu/store/' || substr(cast(sha1 as varchar(80)), 3, 2) 
     || '/' || right(cast(sha1 as varchar(80)), -4) 
  || ' /nyu/stage/ebergelson/' || volume || '/' || container || '/' 
     || a.name || '.' || f.extension[1] 
from slot_asset sa 
  inner join asset a on sa.asset = a.id 
  inner join format f on a.format = f.id
where container = <SLOT ID>;

-- original and unconverted
---- this will retrieve all original assets combined with all assets that didn't need any conversion 
select 
  'cp /nyu/store/' || substr(cast(coalesce(oa.sha1, a.sha1) as varchar(80)), 3, 2) 
     || '/' || right(cast(coalesce(oa.sha1, a.sha1) as varchar(80)), -4) 
  || ' /nyu/stage/ebergelson/' || a.volume || '/' || sa.container || '/' 
     || coalesce(oa.name, a.name || '.' || f.extension[1])
from slot_asset sa 
  inner join asset a on sa.asset = a.id  
  inner join format f on a.format = f.id
  left join transcode t on t.asset = a.id
  left join asset oa on t.orig = oa.id
where sa.container = <SLOT ID>;


-- Kanishka: log into two iTerm2 windows, and connect to database
-- df -h to check for available space in /nyu
-- Joy: edit code above to replace <SLOT ID> with slot number (remove <> and no need for volume)
-- Copy code to iTerm window, paste, then copy generated lines
-- In second iTerm2 window, open nano in /tmp
-- first line: set -x (to echo command, as they take time to run)
-- next line: mkdir -p [path] at top of script ... DO NOT FORGET /NYU/STAGE!!!!!!
-- paste lines and save as file name = slotID.sh (copy into shell script) 
-- bash scriptname.sh  - can take up to ten minutes to complete, watch top
-- ls -1 [path] to list out all copied files

-- Run only converted or original at any given time, not both. okay to run the other at a later time.
-- If you run original after converted, all the files that don't go through conversion will show errors on copy attempt.

-- TODOs:
--  - identify any converted with extension repeated in asset name
--  - ...
