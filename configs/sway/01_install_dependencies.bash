#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Basic sway packages.
install_packages sway swayidle waybar
install_aur_packages swaylock-effects-git

# For screenshot management.
install_packages wl-clipboard grim slurp

# General tools for parsing
install_packages jq

# Brightness settings.
install_packages brightnessctl

# Xorg compatibility until all used program is ported to pure wayland..
install_packages xorg-xwayland
