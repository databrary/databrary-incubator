#!/usr/bin/env bash
set -e
BRANCH="master"
TGT1=$1
rm -rf ~/src/databrary-$BRANCH
mkdir -p ~/src 
cd ~/src
git clone --branch $BRANCH --depth 1 https://github.com/databrary/databrary databrary-$BRANCH
cd ~/src/databrary-$BRANCH
# trigger config file creation
./build-deploy-copy-prep $TGT1
