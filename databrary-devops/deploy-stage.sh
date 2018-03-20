#!/usr/bin/env bash
set -e
ENVIR="stage"
BRANCH=$1
COPYUSR=`whoami`
BASEDIR=~/src
clone_dir="$BASEDIR/databrary-$ENVIR-$BRANCH"  # TODO: lowercase branch name to workaround gargoyle/network bug
rm -rf $BASEDIR/databrary-$ENVIR-*
mkdir -p $BASEDIR 
git clone --branch $BRANCH --depth 1 https://github.com/databrary/databrary $clone_dir
cd $clone_dir
# trigger config file creation
TMPDIR=/tmp ./build-package-copy-prep-stage $COPYUSR
