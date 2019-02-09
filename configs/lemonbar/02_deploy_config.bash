#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./scripts/lemonbar-main.bash ${HOME}/.scripts/my-lemonbar \
  ./scripts/lemonbar-update.bash ${HOME}/.scripts/my-lemonbar-update
