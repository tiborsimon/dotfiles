#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/kitty.conf ${HOME}/.config/kitty/kitty.conf \
  ./config/themes/japanesque.conf ${HOME}/.config/kitty/theme.conf

link_scripts kitty \
  ./scripts/color_table.sh \
