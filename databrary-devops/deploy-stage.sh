#!/usr/bin/env bash
set -e
ENVIR="stage"
BRANCH="develop"
COPYUSR=`whoami`
BASEDIR=~/src
clone_dir="$BASEDIR/databrary-$ENVIR-$BRANCH"
rm -rf $clone_dir
mkdir -p $BASEDIR 
git clone --branch $BRANCH --depth 1 https://github.com/databrary/databrary $clone_dir
cd $clone_dir
# trigger config file creation
TMPDIR=/tmp ./build-package-copy-prep-stage $COPYUSR
