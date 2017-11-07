#!/bin/bash                                                                     
# expects two arguments:
#   1. the email address to alert
#   2. the number of stale entries in uploads to consider as inactive
# ensure running as databrary (or demo, respectively) to connect to proper db
# set these appropriately:
src_path="/home/build/databrary"
conf_dir="/home/databrary"

cd $conf_dir
while true
do
   res=`echo "select count(*) from upload" | $src_path/runsql - | head -3 | grep '\s*$2\s*'`
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
