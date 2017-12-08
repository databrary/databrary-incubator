#!/usr/bin/env bash
set -e
BRANCH="master"
BASEDIR=~/dev
rm -rf $BASEDIR/databrary-$BRANCH
mkdir -p $BASEDIR 
cd $BASEDIR
git clone --branch $BRANCH --depth 1 https://github.com/databrary/databrary databrary-$BRANCH
cd databrary-$BRANCH
TMPDIR=/tmp nix-build --attr databrary --show-trace --no-out-link --keep-failed default.nix
