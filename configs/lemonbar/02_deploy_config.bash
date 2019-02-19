#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_scripts lemonbar \
  ./scripts/main.bash \
  ./scripts/update.bash \
  ./scripts/init.bash \
  ./scripts/monitor.bash \
  ./scripts/scheduler.bash

link_package \
  ./config/lemonbar.service ${HOME}/.config/systemd/user/lemonbar.service \
  ./config/lemonbar-scheduler.service ${HOME}/.config/systemd/user/lemonbar-scheduler.service \
  ./config/lemonbar-scheduler.timer ${HOME}/.config/systemd/user/lemonbar-scheduler.timer
