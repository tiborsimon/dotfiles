#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# Basic sway packages.
install_packages sway swayidle swaylock

# For screenshot management.
install_packages wl-clipboard grim slurp

# General tools for parsing
install_packages jq

# Brightness settings.
install_packages brightnessctl
