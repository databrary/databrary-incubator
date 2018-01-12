#!/usr/bin/env bash
set -e
ssh -t $1@devdatabrary2.home.nyu.edu " \
     cd ~ \
  && ~/databrary-incubator/databrary-devops/deploy-prod.sh \
"
