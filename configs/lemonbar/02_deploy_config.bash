#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_scripts lemonbar \
  ./scripts/main.bash \
  ./scripts/update.bash \
  ./scripts/init.bash \
  ./scripts/monitor.bash

link_package \
  ./config/lemonbar.service ${HOME}/.config/systemd/user/lemonbar.service \
  ./config/lemonbar-clock.service ${HOME}/.config/systemd/user/lemonbar-clock.service \
  ./config/lemonbar-clock.timer ${HOME}/.config/systemd/user/lemonbar-clock.timer
