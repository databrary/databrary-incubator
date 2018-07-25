#!/usr/bin/env bash
set -e
ssh -t datadeploy@devdatabrary2.home.nyu.edu " \
     cd ~ \
  && ~/databrary-incubator/databrary-devops/deploy-prod.sh \
"
