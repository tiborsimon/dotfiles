#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package xorg \
             ./config/xinitrc ${HOME}/.xinitrc \
             ./config/20-intel.conf /etc/X11/xorg.conf.d/20-intel.conf
