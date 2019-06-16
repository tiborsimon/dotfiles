#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# General tools
install_packages bc tree htop wget

# Skype - keychain needed to be able to stay logged in.
install_aur_packages skypeforlinux-stable-bin
install_packages gnome-keychain

# Screenshot
install_packages maim

# DNS related tools
install_packages dnsutils

# Networking packages
install_packages traceroute

# Removable media handler
install_packages udisks2

# Media
install_packages vlc

# File sharing
install_packages transmission-gtk
