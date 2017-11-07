#!/bin/bash                                                                     
# expects an argument providing the email address to alert

while true
do
   res=`echo "select count(*) from upload" | ./runsql - | head -3 | grep '\s*0\s*'`
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
