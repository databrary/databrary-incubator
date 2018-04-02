#!/usr/bin/env bash
set -e

cd /home/demo

# run any new migrations, if any
./runNewDbMigrations.sh

# clear out migrations script for next run
mv runNewDbMigrations.sh runNewDbMigrations.sh.last
echo 'cd $(dirname $(readlink /home/demo/databraryExeLink))/../share/x86_64-linux-ghc-8.0.2/databrary-1/schema/' > /home/demo/runNewDbMigrations.sh

# print out timestamp of latest build
stat -c '%y %N' databraryExeLink

# start up web application and its child processes
./databraryExeLink
