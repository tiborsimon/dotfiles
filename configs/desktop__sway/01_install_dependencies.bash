#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Basic sway packages.
install_packages sway swayidle wayland-protocols
install_aur_packages swaylock-effects-git

# For screenshot management.
install_packages wl-clipboard grim slurp

# General tools for parsing
install_packages jq

# Brightness settings.
install_packages brightnessctl

# Notification manager
install_packages mako libnotify

# Audio control
install_packages pulseaudio pulseaudio-alsa

# Xorg compatibility until all used program is ported to pure wayland..
# This is needed only for plantuml AFAIK.
# install_packages xorg-xwayland
