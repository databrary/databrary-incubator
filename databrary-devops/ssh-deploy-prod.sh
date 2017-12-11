#!/usr/bin/env bash
set -e
ssh -t $1@devdatabrary2.home.nyu.edu " \
     cd ~ \
  && rm -rf ~/databrary-incubator \
  && git clone http://github.com/databrary/databrary-incubator \
  && ~/databrary-incubator/databrary-devops/deploy-prod.sh \
"
