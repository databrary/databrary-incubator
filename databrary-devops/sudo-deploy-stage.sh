#!/usr/bin/env bash
set -e
sudo su - ka988 -c " cd ~ && rm -rf ~/databrary-incubator && git clone http://github.com/databrary/databrary-incubator && ~/databrary-incubator/databrary-devops/deploy-stage.sh $1 "
