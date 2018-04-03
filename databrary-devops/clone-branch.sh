#!/bin/bash
set -e

branch=$1
src_dir=/home/centos/src

cd $src_dir

echo "==== Delete prior cloned branch, database, and uploads"
rm -rf $src_dir/databrary

echo "==== Clone single branch $branch to $src_dir/databrary with history trimmed to last 5 commits"
git clone http://github.com/databrary/databrary --branch $branch --depth 5
