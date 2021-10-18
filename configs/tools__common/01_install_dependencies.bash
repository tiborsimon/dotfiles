#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Package for 'lsusb'.
install_packages usbutils

# For battery tools.
install_packages upower

# Various packages.
install_packages tree archey3
