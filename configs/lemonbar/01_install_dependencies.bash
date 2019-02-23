#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# for backlight adjustments
install_packages xorg-xbacklight

install_aur_packages lemonbar-xft-git
