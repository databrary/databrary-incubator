#!/usr/bin/env bats

# FIXME: Work in progress.
#
# The intent of this script is to create a series of curl tests that run against
# the local dev site. The first example checks for the Date header that gets
# sent with every request. That might sound silly, but currently it's Databrary
# source code that sends that header, so we have to check for it (even though
# we're moving to let library code handle this).
#
# I began this script while working to convert routes to be served by Servant.
#
# This script uses the bats framework: https://github.com/bats-core/bats-core

url=http://localhost:8000

@test "date header" {
    # Hopefully date and curl run in the same minute. :)
    pattern=$(date -u +"^Date: %a, %d %b %Y %R:.. GMT")
    curl -sI ${url}/user/login | grep "$pattern"
}
@test "cache header" {
    route=
    pattern="^Cache-Control: no-cache"
    ! curl -sI $url | grep -q "$pattern"
    curl -sI -H "X-Requested-With: DatabraryClient" $url | grep -q "$pattern"
}
