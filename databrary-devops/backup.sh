#!/bin/sh
# 0       4       *       *       *       $HOME/backup
umask 027
pg_dump -f /nyu/dbdump/`date +%FT%T` -h localhost -Fc databrary
