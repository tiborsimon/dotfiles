#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/udevmon.yaml /etc/udevmon.yaml

execute_with_privilege cp --force ./config/udevmon.service /usr/lib/systemd/system/udevmon.service

link_scripts keyboard \
  ./scripts/reset.bash
