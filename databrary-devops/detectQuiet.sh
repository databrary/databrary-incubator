#!/bin/bash                                                                     
# expects one arguments:
#   1. the email address to alert
# ensure running as databrary (or demo, respectively) to connect to proper db (use whoami)

while true
do
   # upload tokens expire a week after creation, so one that expire in more than 6 days from now are probably active
   # third line is the actual count
   # store result in res to avoid printing output
   res=`echo "select count(*) from upload where expires > CURRENT_TIMESTAMP + interval '6 days'" | psql -U databrary databrary | head -3 | grep '\s*0\s*'`
   if [ $? == 0 ]
   then
     echo "No uploads, alerting."
     echo "Subject: Uploads have stopped" | sendmail -v $1                                   
     exit 0
   else
     echo "Uploads in progress."
   fi
   sleep 5
done
