#!/usr/bin/env bash
set -e
ENVIR="prod"
BRANCH="master"
COPYUSR=`whoami`
BASEDIR=~/src
clone_dir="$BASEDIR/databrary-$ENVIR-$BRANCH"
rm -rf $BASEDIR/databrary-$ENVIR-*
mkdir -p $BASEDIR 
git clone --branch $BRANCH --depth 1 https://github.com/databrary/databrary $clone_dir
cd $clone_dir
git log -1
# trigger config file creation
TMPDIR=/tmp ./build-package-copy-prep-prod $COPYUSR
