#!/usr/bin/env bash
set -e

cd /home/databrary

# run any new migrations, if any
./runNewDbMigrations.sh

# clear out migrations script, prepare for next run
mv runNewDbMigrations.sh runNewDbMigrations.sh.last
echo 'cd $(dirname $(readlink /home/databrary/databraryExeLink))/../share/x86_64-linux-ghc-8.0.2/databrary-1/schema/' > /home/databrary/runNewDbMigrations.shchmod +x /home/demo/runNewDbMigrations.sh
chmod +x /home/databrary/runNewDbMigrations.sh

# print out timestamp of latest build
stat -c '%y %N' databraryExeLink

# start up web application and its child processes
./databraryExeLink
