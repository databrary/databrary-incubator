#!/bin/sh
for f in /nyu/dbdump/`date +%F`T04:00:* ; do
        today=$f
done
dump=${1:-$today}
if [[ ! -r $dump ]] ; then
        echo "Current dump ($today) not found."
        echo "Usage: $0 [DUMP]"
        exit 1
fi
psql -d demo -h localhost -f reset.sql
pg_restore -d demo -h localhost -O $dump
rm -rf /nyu/demo/store/* /nyu/demo/upload/*
rm -f hpc/databrary/demo/*
ssh hpc rm -f databrary/demo/\*  ## TODO: should this use hpcprince?
