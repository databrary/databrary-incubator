-- update start date as desired
select orig, asset, owner, segment, start, process, substring(log from 0 for 50) 
from transcode 
where process is not null 
and start > '2018-06-10' 
order by start desc;
