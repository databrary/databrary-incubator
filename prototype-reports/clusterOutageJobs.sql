-- when process is null and (asset is missing size or something?) and log message is "prince is down", 
--  then those are the failures

select orig, asset, owner, segment, start, process, substring(log from 0 for 50) 
from transcode 
where start > '2018-06-10'  -- update start date as desired
order by start desc;

