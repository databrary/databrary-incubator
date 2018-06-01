#!/usr/bin/env bats

# FIXME: Work in progress (does nothing yet).
#
# The intent of this script is to create a series of curl tests that run against
# the local dev site. The first example hits /user/login and looks for some text
# that is unique to that route. At least, I hope that's what this text is.
#
# I began this script while working to convert routes to be served by Servant.
#
# This script uses the bats framework: https://github.com/bats-core/bats-core

declare -A tests
tests=(
    # FIXME: This is *not* specific to this route. It's part of the generic
    # payload for the Angular app.
    ['user/login']='window.$play={user:{"id":-1,"sortname":"Nobody","institution":true,"authorization":0}};'
)

for route in "${!tests[@]}"; do
    url="localhost:8000/${route}"
    # Note: Trying to use associated array indexing within the test triggers a
    # bats bug. Outside is fine, however.
    string="${tests[$route]}"
    @test "$route" {
        curl -s "$url" | grep -qF "$string"
    }
done
