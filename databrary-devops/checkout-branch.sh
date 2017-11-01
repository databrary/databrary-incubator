#!/bin/bash
set -e

build_user=centos
cd /home/$build_user/src/databrary
git stash save
git fetch
git checkout $1
git rev-parse --abbrev-ref HEAD > ~/src/databrary-branch.txt
git pull
