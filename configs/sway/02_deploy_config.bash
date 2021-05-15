#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/config ${HOME}/.config/sway/config \
  ./config/status.sh ${HOME}/.config/sway/status.sh