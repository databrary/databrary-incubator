-- when process is null and (asset is missing size or something?) and log message is "prince is down", "Authentication failed."
--  then those are the failures

select orig, asset, owner, segment, start, process, substring(log from 0 for 50) 
from transcode 
where start > '2018-06-10'  -- update start date as desired
order by start desc;

--- short list to observe jobs in progress; 
--- for items where process is null, observe the log or asset size to see if failed or complete
select asset, owner, start, process, substring(log from 0 for 20) 
from transcode 
where log like '%Prince%' and start > '2018-06-10'  
order by start desc;

--- change # to start a job (doesn't this need a cookie header??)
curl 'https://nyu.databrary.org/admin/transcode/132779' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  --data 'action=start'
