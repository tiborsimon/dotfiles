#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

execute systemctl --user enable lemonbar-scheduler.timer

if ! systemctl is-enabled acpid &>/dev/null
then
  execute_with_privilege systemctl enable acpid
fi
