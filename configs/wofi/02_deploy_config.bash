#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/config ${HOME}/.config/wofi/config \
  ./config/style.css ${HOME}/.config/wofi/style.css
