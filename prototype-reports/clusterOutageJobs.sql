# TODO: turn this login sequence into one line
ssh <NETID>@<BASTION>
ssh -t <PROD> "sudo -i su - <APP_USER>"
psql -h localhost

-- when process is null and (asset is missing size or something?) and log message is "prince is down", "Authentication failed."
--  then those are the failures

select orig, asset, owner, segment, start, process, substring(log from 0 for 50) 
from transcode 
where start > '2018-08-05'  -- update start date as desired
order by start desc;

--- short list to gather failed jobs; 
--- for items where process is null, observe the log or asset size to see if failed or complete
select asset, owner, start, process, substring(log from 0 for 20) 
from transcode 
where log like '%Authentication failed%' and start > '2018-08-05' 
order by start desc;

-- login using curl and save local cookie
source /tmp/pwenv.sh  #export PASSWD here

curl 'https://nyu.databrary.org/api/user/login' \
  -H 'Content-Type: application/json;charset=UTF-8' \
  -d '{"email":"<SUPER.EMAIL>","password":"$PASSWD","superuser":true}' \
  --cookie-jar /tmp/cookies.txt

--- change # to start a job. TRANSCODE ASSET ID is the "asset" column in queries above
-- this takes about 48 seconds to complete per call. 
-- start all of the failed jobs.
curl 'https://nyu.databrary.org/admin/transcode/<TRANSCODE ASSET ID>' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  --data 'action=start' \
  --cookie-jar /tmp/cookies.txt
  
rm /tmp/cookies.txt

--- monitor progress
--- to be very thorough, once should also 
---   observe the log or asset size to see if failed or complete
--  this only detects if an active job is still running, assume no active job means completed successfully.
select asset, owner, start, process, substring(log from 0 for 20) 
from transcode 
where log like '%Authentication failed%' and start > '2018-08-05' and process is not null
order by start desc;

-- each job takes about one hour to complete, sometimes up to three hours. 
-- the jobs run in parallel.
