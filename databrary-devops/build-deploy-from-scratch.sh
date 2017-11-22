#!/usr/bin/env bash
set -e
BRANCH="master"
TGT1=$1
BASEDIR=~/src
rm -rf $BASEDIR/databrary-$BRANCH
mkdir -p $BASEDIR 
cd $BASEDIR
git clone --branch $BRANCH --depth 1 https://github.com/databrary/databrary databrary-$BRANCH
cd databrary-$BRANCH
# trigger config file creation
./build-package-copy-prep $TGT1
