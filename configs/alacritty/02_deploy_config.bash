#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/alacritty.yml ${HOME}/.config/alacritty/alacritty.yml
