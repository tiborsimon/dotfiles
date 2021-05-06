#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# JSON processing
install_packages jq

# Package for 'lsusb'.
install_packages usbutils

# For easy brightness control.
install_packages brightnessctl
