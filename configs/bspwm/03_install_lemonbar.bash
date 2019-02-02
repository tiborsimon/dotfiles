#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages ttf-font-awesome
install_aur_packages lemonbar-xft-git
