#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

execute systemctl --user enable lemonbar-scheduler.timer
