#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

execute_with_privilege sudo cp -vf ./config/udevmon.yaml /etc/interception/udevmon.yaml
execute_with_privilege sudo systemctl enable udevmon
execute_with_privilege sudo systemctl start udevmon
execute_with_privilege sudo systemctl status udevmon
