#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

run_with_privilege systemctl enable udevmon.service
run_with_privilege systemctl restart udevmon.service
