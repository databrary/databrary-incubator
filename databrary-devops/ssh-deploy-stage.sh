#!/usr/bin/env bash
# should take optional branch name as a parameter, and default to develop when nothing provided
set -e
ssh -t $1@devdatabrary2.home.nyu.edu " \
     cd ~ \
  && rm -rf ~/databrary-incubator \
  && git clone http://github.com/databrary/databrary-incubator \
  && ~/databrary-incubator/databrary-devops/deploy-stage.sh $2 \
"
