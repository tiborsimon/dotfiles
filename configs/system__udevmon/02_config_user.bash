#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

execute_with_privilege sudo cp -vf \
  './config/udevmon.yaml' '/etc/interception/udevmon.yaml'

if [ "$(systemctl is-enabled udevmon)" != 'enabled' ]
then
  execute_with_privilege sudo systemctl enable udevmon
fi

if [ "$(systemctl is-active udevmon)" != 'active' ]
then
  execute_with_privilege sudo systemctl start udevmon
  execute_with_privilege sudo systemctl status udevmon
fi
